module Hercules.Sync.GitHub
  ( syncAllRepos
  , syncRepo
  , PullRequest(..)
  ) where

import GitHub.Endpoints.PullRequests
import GitHub.Endpoints.Repos
import Say
import Data.Vector (Vector)
import Data.Text (Text)
import Control.Monad


syncAllRepos :: Name Owner -> IO ()
syncAllRepos owner = do
  res <- userRepos owner RepoPublicityOwner
  case res of
    Right repos -> do
      updateListRepos repos
      forM_ repos $ \repo -> syncPullRequests owner (repoName repo)
      syncBranchList
    Left err -> logError err
  return ()

-- | Fetch pull requests for a repo and update the database table.
syncPullRequests :: Name Owner -> Name Repo -> IO ()
syncPullRequests owner repo = do
  res <- pullRequestsFor owner repo
  case res of
    Right prs -> updateListPullRequests prs
    Left err -> logError err

-- | Update database table with repos
updateListRepos :: Vector Repo -> IO ()
updateListRepos = undefined

-- | Update database table with pull requests
updateListPullRequests :: Vector SimplePullRequest -> IO ()
updateListPullRequests = undefined

-- for all repos,
syncBranchList :: IO ()
syncBranchList = undefined

logError :: Error -> IO ()
logError = sayErrShow

syncRepo :: Text -> IO ()
syncRepo _ =  return ()
