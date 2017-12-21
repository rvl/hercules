{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Hercules.Sync.GitHub
  ( syncAllReposByOwner
  , syncAllReposByOwnerId
  , syncRepo
  , PullRequest(..)
  ) where

import GitHub.Endpoints.PullRequests hiding (User(..))
import GitHub.Endpoints.Repos hiding (User(..))
import Say
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad
import Data.Proxy
import Control.Monad.Log
import Data.Monoid
import Data.Maybe (fromMaybe)

import Hercules.Database.Hercules (GithubRepo'(..), GithubPullRequest'(..), userGithubId, User)
import Hercules.Query.Hercules
import Hercules.ServerEnv
import Hercules.OAuth.User
import Hercules.Log

-- | Sync all public repos for which the user is an owner.
syncAllReposByOwner :: Name Owner -> App ()
syncAllReposByOwner owner = do
  res <- liftIO $ userRepos owner RepoPublicityOwner
  case res of
    Right repos -> do
      updateListRepos repos
      mapM_ syncPullRequests repos
      syncBranchList
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
updateListRepos :: Vector Repo -> App ()
updateListRepos rs = withHerculesConnection $ flip addUpdateGitHubRepos rs'
  where
    rs' = map makeRepo (V.toList rs)
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

-- for all enabled repos, update their git clones and store branch names in db
syncBranchList :: App ()
syncBranchList = undefined

syncRepo :: Text -> App ()
syncRepo _ =  return ()

gitHubErrMsg :: Error -> LogMessage
gitHubErrMsg = LogString . T.pack . show
