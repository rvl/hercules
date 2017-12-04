{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE LambdaCase        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hercules.Hooks.GitHub
  ( GitHubAppAPI
  , gitHubWebHookPR
  , gitHubWebHookPing
  , gitHubWebHookCtx
  , GitHubKey
  , PingRegistration(..)
  ) where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad.Log
import Data.Monoid
import qualified Data.ByteString.Lazy as BL
import Safe
import Data.Foldable
import Data.Maybe (fromMaybe)

import           GitHub.Data.PullRequests
import           GitHub.Data.Statuses
import           GitHub.Data.Repos
import           GitHub.Data.Webhooks
import qualified GitHub.Data.PullRequests        as GH
import qualified GitHub.Data.Repos               as GH
import qualified GitHub.Data.Options             as GH
import qualified GitHub.Data.Definitions         as GH
import qualified GitHub.Data.Statuses            as GH
import qualified GitHub.Data.URL                 as GH
import qualified GitHub.Data.Id                  as GH
import qualified GitHub.Data.Name                as GH
import qualified GitHub.Endpoints.Repos.Statuses as GH

import           GitHub.Request (executeRequestWithMgr)
import GitHub.Endpoints.PullRequests hiding (User)

import Data.Swagger (ToSchema(..))
import Servant
import Servant.Swagger
import Servant.Server
import Servant.GitHub.Webhook
import Data.Aeson

import Hercules.ServerEnv
import Hercules.Log
import Hercules.Query.Hercules
import Hercules.Database.Hercules
import Hercules.Encryption

type GitHubAppAPI = "github" :> "webhook" :> (
  GitHubEvent '[ 'WebhookPullRequestEvent ]
    :> GitHubSignedReqBody '[JSON] PullRequestEvent
    :> Post '[JSON] NoContent
    :<|>
  GitHubEvent '[ 'WebhookPingEvent ]
    :> GitHubSignedReqBody '[JSON] PingRegistration
    :> Post '[JSON] NoContent
  )

data PingRegistration = PingRegistration
                        { pingAppId :: Int
                        } deriving (Show)

instance FromJSON PingRegistration where
  parseJSON v = PingRegistration <$> parseAppId v
    where
      parseAppId = withObject "ping event" $ \o -> do
        h <- o .: "hook"
        h .: "app_id"

-- | Ping is used during github app registration.
-- App ID is required for authenticating the app, so keep it in the db.
-- https://developer.github.com/webhooks/#ping-event
-- https://developer.github.com/apps/building-integrations/setting-up-and-registering-github-apps/about-authentication-options-for-github-apps/#authenticating-as-a-github-app
gitHubWebHookPing :: RepoWebhookEvent -> ((), PingRegistration) -> App NoContent
gitHubWebHookPing _ (_, ev@PingRegistration{..}) = do
  logInfo . LogString $ "Received GitHub ping: " <> T.pack (show ev)
  withHerculesConnection $ \c -> setGitHubAppId c pingAppId
  return NoContent


github :: GH.Auth -> GH.Request k a -> App (Either GH.Error a)
github token req = withHttpManager (\mgr -> executeRequestWithMgr mgr token req)

findUserToken :: GH.SimpleOwner -> App GH.Auth
findUserToken owner = do
  let query = userGitHubIdQuery . T.pack . show . GH.untagId . GH.simpleOwnerId $ owner
  user <- headMay <$> runHerculesQueryWithConnection query :: App (Maybe User)
  case user >>= userGithubToken of
    Just encryptedToken -> GH.OAuth <$> decrypt encryptedToken
    Nothing -> throwError $ err503 { errBody = "no token for repo owner" }

buildEvents :: [PullRequestEventType]
buildEvents = [PullRequestOpened, PullRequestReopened, PullRequestSynchronized]

gitHubWebHookPR :: RepoWebhookEvent -> ((), PullRequestEvent) -> App NoContent
gitHubWebHookPR _ (_, ev@PullRequestEvent{..}) =  do
  logInfo . LogString $ "Received GitHub pull request notification " <> T.pack (show ev)

  when (elem pullRequestEventAction buildEvents) $ do
    let
      owner = GH.simpleOwnerLogin . repoOwner $ pullRequestRepository
      name = repoName pullRequestRepository

    -- currently using the user's token, but we could also/should use the app's token.
    user <- findUserToken (repoOwner pullRequestRepository)

    -- get commits revisions for pull request
    Right commits <- github user $ pullRequestCommitsR owner name
               (pullRequestId pullRequestEventPullRequest) FetchAll

    forM_ (map commitSha (toList commits)) $ \sha -> do
      let updateStatus st = void . github user $ GH.createStatusR owner name sha st
          queued s m = NewStatus s Nothing (Just m) Nothing
          complete (Left err) url = NewStatus StatusFailure (Just url) (Just err) Nothing
          complete (Right msg) url = NewStatus StatusSuccess (Just url) (Just msg) Nothing
          completeHandler res url = updateStatus (complete res url)

      updateStatus $ queued StatusPending "Hercules build is being scheduled"

      addBuild owner name sha completeHandler >>= \case
        True -> updateStatus $ queued StatusPending "Hercules build has been queued"
        False -> updateStatus $ queued StatusError "Scheduling Hercules build failed"

  return NoContent

addBuild :: Name Owner -> Name Repo -> Name Commit
         -> (Either Text Text -> URL -> App ())
         -> App Bool
addBuild owner repo rev complete = return True

gitHubWebHookCtx :: Env -> GitHubKey
gitHubWebHookCtx = gitHubKey . pure . fromMaybe "" . envGitHubWebHookSecret

----------------------------------------------------------------------------
-- all instances required to document webhook endpoints in swagger...

instance ToSchema GH.URL
instance ToSchema GH.IssueState
instance ToSchema GH.Language
instance ToSchema GH.MergeableState
instance ToSchema GH.OwnerType
instance ToSchema GH.PullRequest
instance ToSchema GH.PullRequestCommit
instance ToSchema GH.PullRequestEvent
instance ToSchema GH.PullRequestEventType
instance ToSchema GH.PullRequestLinks
instance ToSchema GH.Repo
instance ToSchema GH.RepoRef
instance ToSchema GH.SimpleOwner
instance ToSchema GH.SimpleUser
instance ToSchema (GH.Id GH.Owner)
instance ToSchema (GH.Id GH.PullRequest)
instance ToSchema (GH.Id GH.Repo)
instance ToSchema (GH.Id GH.User)
instance ToSchema (GH.Name GH.Owner)
instance ToSchema (GH.Name GH.Repo)
instance ToSchema (GH.Name GH.User)

instance HasSwagger GitHubEvent where
  toSwagger _ = undefined -- fixme: swagger landmine
