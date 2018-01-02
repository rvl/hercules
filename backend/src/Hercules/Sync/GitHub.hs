{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase, ScopedTypeVariables #-}

module Hercules.Sync.GitHub
  ( syncAllReposByOwner
  , syncAllReposByOwnerId
  , syncRepo
  , syncRepoBranch
  , addJobsetGitHub
  , PullRequest(..)
  ) where

import GitHub.Endpoints.PullRequests hiding (User(..))
import GitHub.Endpoints.Repos hiding (User(..))
import GitHub.Endpoints.Extra
import Say
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Aeson (Value)
import Data.ByteString(ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad
import Data.Proxy
import Control.Monad.Log
import Data.Monoid
import Data.Maybe (fromMaybe)
import Crypto.JWT hiding (Error)
import Control.Monad.Reader
import Control.Monad.Except
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Network.URI (URI(..), URIAuth(..))
import Control.Lens
import Data.Yaml                (decodeEither', prettyPrintParseException)

import Hercules.Database.Hercules (GithubRepo'(..), GithubRepo, GithubPullRequest'(..), userGithubId, User)
import Hercules.Query.Hercules
import Hercules.ServerEnv
import Hercules.OAuth.User
import Hercules.Log
import Hercules.Input.Git (fetchRepoGit, getBuildSpec, getRevision)

-- | Sync all public repos for which the user is an owner.
syncAllReposByOwner :: Name Owner -> App ()
syncAllReposByOwner owner = do
  res <- liftIO $ userRepos owner RepoPublicityOwner
  case fmap V.toList res of
    Right repos -> do
      repos' <- updateListRepos repos
      let enabled = [r | (r, r') <- zip repos repos', githubRepoEnabled r']
      mapM_ syncPullRequests enabled
      mapM_ syncBranchList enabled
    Left err -> logError (gitHubErrMsg err)
  return ()

syncAllReposByOwnerId :: UserId -> App ()
syncAllReposByOwnerId u = do
  user <- runHerculesQueryWithConnectionSingular (userIdQuery u) :: App (Maybe User)
  case user >>= userGithubId of
    Just userId -> syncAllReposByOwner (mkName (Proxy :: Proxy Owner) userId)
    Nothing -> return ()

-- | Fetch pull requests for a repo and update the database table.
syncPullRequests :: Repo -> App ()
syncPullRequests Repo{..} = do
  res <- liftIO $ pullRequestsFor (simpleOwnerLogin repoOwner) repoName
  case res of
    Right prs -> updateListPullRequests repoId prs
    Left err -> logError (gitHubErrMsg err)

-- | Update database table with repos
updateListRepos :: [Repo] -> App [GithubRepo]
updateListRepos rs = withHerculesConnection $ flip addUpdateGitHubRepos rs'
  where
    rs' = map makeRepo rs
    makeRepo r@Repo{..} = GithubRepo (untagId repoId) (untagName repoName)
                          (repoFullName r)
                          (fromMaybe "" repoDefaultBranch)
                          (T.pack $ show repoUrl)
                          False

-- | Make owner/repo string from Repo data.
repoFullName :: Repo -> Text
repoFullName Repo{..} = owner <> "/" <> untagName repoName
  where owner = untagName (simpleOwnerLogin repoOwner)

-- | Update database table with pull requests
updateListPullRequests :: Id Repo -> Vector SimplePullRequest -> App ()
updateListPullRequests repo prs = withHerculesConnection $ flip addUpdateGitHubPullRequests prs'
  where
    prs' = map makePR (V.toList prs)
    makePR SimplePullRequest{..} = GithubPullRequest simplePullRequestNumber (untagId repo) simplePullRequestTitle

syncBranchList :: Repo -> App ()
syncBranchList Repo{..} = do
  res <- liftIO $ branchesFor (simpleOwnerLogin repoOwner) repoName
  case res of
    Right bs -> return () -- fixme: implement sync to database
    Left err -> logError (gitHubErrMsg err)

-- fixme: runExceptT and ExceptT String App a
syncRepo :: GithubRepo -> App FilePath
syncRepo repo = do
  iss <- withHerculesConnection getGitHubAppId >>=
    maybe (fail "No app registered") (pure . mkId Proxy)
  jwt <- repoJWT iss
  cloneUri <- repoCloneURI repo iss jwt
  liftIO $ fetchRepoGit cloneUri

syncRepoBranch :: GithubRepo -> Text -> App (FilePath, Text, Maybe Value)
syncRepoBranch repo branchName = do
  clonePath <- syncRepo repo
  liftIO $ syncBranch branchName clonePath

syncBranch :: Text -> FilePath -> IO (FilePath, Text, Maybe Value)
syncBranch branchName clonePath = getBuildSpec branchName clonePath >>= \case
  Left e -> fail e
  Right yaml -> case decodeEither' (BL.toStrict yaml) of
    Left e -> fail $ "parsing yaml: " ++ prettyPrintParseException e
    Right spec -> getRevision branchName clonePath >>= \case
      Nothing -> fail "Could not get head revision id from repo" -- fixme: more diagnostic info
      Just rev -> return (clonePath, rev, spec)

addJobsetGitHub :: GithubRepo -> Text -> FilePath -> Text -> Value -> App ()
addJobsetGitHub repo branchName clonePath rev spec =
  withHerculesConnection $ \c -> withTransaction c $ do
    updateGitHubRepoCache c (githubRepoId repo) clonePath
    void $ updateJobsetBranch c (githubRepoId repo) branchName rev spec

repoCloneURI :: GithubRepo -> Id Installation -> SignedJWT -> App URI
repoCloneURI repo appId jwt = do
  let jwt' = BL.toStrict $ encodeCompact jwt
  liftIO $ createInstallationAccessToken' jwt' appId >>= \case
    Left err -> fail "blah blah"
    Right token -> return $ repoURI (tokenText token) repo

repoURI :: ByteString -> GithubRepo -> URI
repoURI token GithubRepo{..} = URI "https" (Just auth) (T.unpack path) "" ""
  where
    auth = URIAuth ("x-access-token:" <> S8.unpack token) "github.com" ""
    path = "/" <> githubRepoFullName <> ".git"

repoJWT :: Id Installation -> App SignedJWT
repoJWT appId = do
  claims <- repoJWTClaims appId
  asks envGitHubAppPrivateKey >>= \case
    Just k -> do
      res <- liftIO . runExceptT $ signClaims k (newJWSHeader ((), RS256)) claims
      -- fixme: better error handling, show error
      case res of
        Left (err :: JWTError) -> fail "Error signing GitHub request"
        Right t -> return t
    Nothing -> fail "GitHub app private key not configured"

repoJWTClaims :: Id Installation -> App ClaimsSet
repoJWTClaims appId = do
  t <- liftIO getCurrentTime
  pure $ emptyClaimsSet
    & claimIat .~ Just (NumericDate t)
    & claimExp .~ Just (NumericDate (gitHubMaxExp t))
    & claimIss .~ (show (untagId appId) ^? stringOrUri)

-- github maximum expiry time is 10 mins
gitHubMaxExp :: UTCTime -> UTCTime
gitHubMaxExp = addUTCTime 600

gitHubErrMsg :: Error -> LogMessage
gitHubErrMsg = LogString . T.pack . show
