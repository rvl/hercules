{-# LANGUAGE Arrows          #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

{-|
A module to handle the different queries we might want to make to Hercules's
database
-}
module Hercules.Query.Hercules
  ( userIdQuery
  , userGitHubIdQuery
  , insertUser
  , findJobsetQuery
  , findJobsetQueryOld
  , prevJobsetEvalQuery
  , jobsetByIdQuery
  , jobsetRestriction
  , jobExists
  ) where

import Control.Arrow              (returnA)
import Data.ByteString
import Data.Text
import Database.PostgreSQL.Simple (Connection)
import Opaleye.Extra

import Hercules.Database.Hercules
import Hercules.OAuth.User

-- | A query to get a user from their id
userIdQuery :: UserId -> Query UserReadColumns
userIdQuery (UserId uid) = proc () -> do
  user@User{..} <- queryTable userTable -< ()
  restrict -< pgInt8 uid .== userId
  returnA -< user

-- | A query to get a user by their github id
userGitHubIdQuery :: Text -> Query UserReadColumns
userGitHubIdQuery githubId = proc () -> do
  user@User{..} <- queryTable userTable -< ()
  restrict -< pgStrictText githubId `eqNullable` userGithubId
  returnA -< user

insertUser :: Connection -> User' a Text Text Text ByteString -> IO (Maybe UserId)
insertUser c User {userName
                  ,userEmail
                  ,userGithubId
                  ,userGithubToken} =
  let user =
        User
          Nothing
          (Just (toNullable (pgStrictText userName)))
          (Just (toNullable (pgStrictText userEmail)))
          (Just (toNullable (pgStrictText userGithubId)))
          (Just (toNullable (pgStrictByteString userGithubToken)))
  in runInsertManyReturning c userTable [user] userId >>=
     \case
       [i] -> pure $ Just (UserId i)
       _ -> pure Nothing

findJobsetQueryOld :: Text -> Text -> Query (JobsetReadColumns, GithubBranchReadColumns, GithubRepoReadColumns)
findJobsetQueryOld p j = proc () -> do
  b@GithubBranch{..} <- queryTable githubBranchTable -< ()
  r@GithubRepo{..} <- queryTable githubRepoTable -< ()
  restrict -< githubRepoName .== pgStrictText p .&& githubBranchName .== pgStrictText j
  js@Jobset{..} <- queryTable jobsetTable -< ()
  restrict -< githubBranchId .=== jobsetBranchId
  returnA -< (js, b, r)

findJobsetQuery :: Text -> Text -> Query (GithubRepoReadColumns, GithubBranchReadColumns)
findJobsetQuery p j = proc () -> do
  r@GithubRepo{..} <- queryTable githubRepoTable -< ()
  b@GithubBranch{..} <- queryTable githubBranchTable -< ()
  restrict -< githubRepoName .== pgStrictText p .&& githubBranchName .== pgStrictText j
  returnA -< (r, b)

jobsetByIdQuery :: Int -> Query JobsetReadColumns
jobsetByIdQuery branchId = proc () -> do
  js <- queryTable jobsetTable -< ()
  restrict -< constant branchId .== jobsetBranchId js
  returnA -< js

-- | Predicate to restrict query by id of the jobset.
jobsetRestriction :: JobsetId -> JobsetReadColumns -> Column PGBool
jobsetRestriction jsId js = constant jsId .== jobsetBranchId js


branchByIdQuery :: Int -> Query GithubBranchReadColumns
branchByIdQuery id = proc () -> do
  b <- queryTable githubBranchTable -< ()
  restrict -< githubBranchId b .== constant id
  returnA -< b

repoByIdQuery :: Int -> Query GithubRepoReadColumns
repoByIdQuery id = proc () -> do
  r <- queryTable githubRepoTable -< ()
  restrict -< githubRepoId r .== constant id
  returnA -< r


-- | fixme: return if exists or count = 1
jobExists :: Job -> Query (Column PGInt8)
jobExists (Job js name) = countRows $ proc () -> do
  j <- queryTable jobTable -< ()
  restrict -< jobJobsetId j .== constant js .&& jobName j .== constant name
  returnA -< j


prevJobsetEvalQuery :: Bool -> JobsetId -> Query JobsetevalReadColumns
prevJobsetEvalQuery hasNewBuilds js = proc () -> do
  jse@Jobseteval{..} <- orderBy (desc jobsetevalId) (queryTable jobsetevalTable) -< ()
  restrict -< jobsetevalJobsetId .== constant js
  restrict -< if hasNewBuilds then jobsetevalHasnewbuilds .=== pgInt4 1 else pgBool True
  returnA -< jse
