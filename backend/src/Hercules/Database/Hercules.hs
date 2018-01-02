{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveGeneric         #-}

module Hercules.Database.Hercules where

import Data.ByteString
import Data.Profunctor
import Data.Profunctor.Product
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import Data.Text
import GHC.Int
import Opaleye                         hiding (fromNullable)
import qualified Data.Aeson as JSON
import Data.Time.Clock (UTCTime)
import           Servant.Elm
import           Data.Aeson
import           GHC.Generics

-- | A newtype around @a -> Maybe b@ to facilitate conversions from the
-- Nullable types.
newtype ToMaybe a b = ToMaybe { unToMaybe :: a -> Maybe b }

instance Profunctor ToMaybe where
  dimap f g (ToMaybe h) = ToMaybe (fmap g . h . f)

instance ProductProfunctor ToMaybe where
  empty = ToMaybe pure
  (ToMaybe f) ***! (ToMaybe g) = ToMaybe (\(x, y) -> (,) <$> f x <*> g y)

-- | This instance makes sure that values which are required in the output are
-- required in the input.
instance Default ToMaybe (Maybe a) a where
  def = ToMaybe id

-- | This instance allows values which are optional in the output to be
-- optional in the input.
instance Default ToMaybe (Maybe a) (Maybe a) where
  def = ToMaybe pure

-- | Convert from any Nullable type by "sequencing" over all the fields.
fromNullable :: Default ToMaybe a b => a -> Maybe b
fromNullable = unToMaybe def

---- Types for table: users ----

data User' c1 c2 c3 c4 c5 =
  User
    { userId          :: c1
    , userName        :: c2
    , userEmail       :: c3
    , userGithubId    :: c4
    , userGithubToken :: c5
    }

type User = User' Int64 (Maybe Text) (Maybe Text) (Maybe Text) (Maybe ByteString)

type UserReadColumns = User' (Column PGInt8) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGBytea))

type UserWriteColumns = User' (Maybe (Column PGInt8)) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGBytea)))

type UserNullableColumns = User' (Column (Nullable PGInt8)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGBytea))

type UserNullable = User' (Maybe Int64) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe ByteString)

fromNullableUser :: UserNullable -> Maybe User
fromNullableUser = fromNullable

$(makeAdaptorAndInstance "pUser" ''User')

userTable :: Table UserWriteColumns UserReadColumns
userTable = TableWithSchema "hercules" "users" (pUser
  User
    { userId = optional "id"
    , userName = optional "name"
    , userEmail = optional "email"
    , userGithubId = optional "github_id"
    , userGithubToken = optional "github_token"
    }
  )

---- Types for table: github_branches ----

data GithubBranch' c1 c2 c3 c4 c5 c6 =
  GithubBranch
    { githubBranchId :: c1
    , githubBranchRepoId :: c2
    , githubBranchName :: c3
    , githubBranchRev :: c4
    , githubBranchSpec :: c5
    , githubBranchPullRequestNumber :: c6
    }

 -- fixme: newtypes for ids
type GithubBranchId = Int
type GithubRepoId = Int

type GithubBranch = GithubBranch' GithubBranchId GithubRepoId Text Text JSON.Value (Maybe Int)

type GithubBranchReadColumns = GithubBranch' (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText) (Column PGJsonb) (Column (Nullable PGInt4))

type GithubBranchWriteColumns = GithubBranch' (Maybe (Column PGInt4)) (Column PGInt4) (Column PGText) (Column PGText) (Column PGJsonb) (Maybe (Column (Nullable PGInt4)))

type GithubBranchNullableColumns = GithubBranch' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGJsonb)) (Column (Nullable PGInt4))

type GithubBranchNullable = GithubBranch' (Maybe GithubBranchId) (Maybe GithubRepoId) (Maybe Text) (Maybe Text) (Maybe JSON.Value) (Maybe Int)

fromNullableGithubBranch :: GithubBranchNullable -> Maybe GithubBranch
fromNullableGithubBranch = fromNullable

$(makeAdaptorAndInstance "pGithubBranch" ''GithubBranch')

githubBranchTable :: Table GithubBranchWriteColumns GithubBranchReadColumns
githubBranchTable = TableWithSchema "hercules" "github_branches" (pGithubBranch
  GithubBranch
    { githubBranchId = optional "id"
    , githubBranchRepoId = required "repo_id"
    , githubBranchName = required "name"
    , githubBranchRev = required "rev"
    , githubBranchSpec = required "spec"
    , githubBranchPullRequestNumber = optional "pull_request_number"
    }
  )

---- Types for table: github_pull_requests ----

data GithubPullRequest' c1 c2 c3 =
  GithubPullRequest
    { githubPullRequestNumber :: c1
    , githubPullRequestRepoId :: c2
    , githubPullRequestTitle :: c3
    }

type GithubPullRequest = GithubPullRequest' Int Int Text

type GithubPullRequestReadColumns = GithubPullRequest' (Column PGInt4) (Column PGInt4) (Column PGText)

type GithubPullRequestWriteColumns = GithubPullRequest' (Column PGInt4) (Column PGInt4) (Column PGText)

type GithubPullRequestNullableColumns = GithubPullRequest' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText))

type GithubPullRequestNullable = GithubPullRequest' (Maybe Int) (Maybe Int) (Maybe Text)

fromNullableGithubPullRequest :: GithubPullRequestNullable -> Maybe GithubPullRequest
fromNullableGithubPullRequest = fromNullable

$(makeAdaptorAndInstance "pGithubPullRequest" ''GithubPullRequest')

githubPullRequestTable :: Table GithubPullRequestWriteColumns GithubPullRequestReadColumns
githubPullRequestTable = TableWithSchema "hercules" "github_pull_requests" (pGithubPullRequest
  GithubPullRequest
    { githubPullRequestNumber = required "number"
    , githubPullRequestRepoId = required "repo_id"
    , githubPullRequestTitle = required "title"
    }
  )

---- Types for table: github_repo_cache ----

data GithubRepoCache' c1 c2 c3 c4 =
  GithubRepoCache
    { githubRepoCacheRepoId :: c1
    , githubRepoCachePath :: c2
    , githubRepoCacheStartFetch :: c3
    , githubRepoCacheLastFetch :: c4
    }

type GithubRepoCache = GithubRepoCache' GithubRepoId Text (Maybe UTCTime) (Maybe UTCTime)

type GithubRepoCacheReadColumns = GithubRepoCache' (Column PGInt4) (Column PGText) (Column (Nullable PGTimestamptz)) (Column (Nullable PGTimestamptz))

type GithubRepoCacheWriteColumns = GithubRepoCache' (Column PGInt4) (Column PGText) (Maybe (Column (Nullable PGTimestamptz))) (Maybe (Column (Nullable PGTimestamptz)))

type GithubRepoCacheNullableColumns = GithubRepoCache' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGTimestamptz)) (Column (Nullable PGTimestamptz))

type GithubRepoCacheNullable = GithubRepoCache' (Maybe GithubRepoId) (Maybe Text) (Maybe UTCTime) (Maybe UTCTime)

fromNullableGithubRepoCache :: GithubRepoCacheNullable -> Maybe GithubRepoCache
fromNullableGithubRepoCache = fromNullable

$(makeAdaptorAndInstance "pGithubRepoCache" ''GithubRepoCache')

githubRepoCacheTable :: Table GithubRepoCacheWriteColumns GithubRepoCacheReadColumns
githubRepoCacheTable = TableWithSchema "hercules" "github_repo_cache" (pGithubRepoCache
  GithubRepoCache
    { githubRepoCacheRepoId = required "repo_id"
    , githubRepoCachePath = required "path"
    , githubRepoCacheStartFetch = optional "start_fetch"
    , githubRepoCacheLastFetch = optional "last_fetch"
    }
  )

instance ToJSON GithubRepo where
instance FromJSON GithubRepo where
instance ElmType GithubRepo where

---- Types for table: github_repos ----

data GithubRepo' c1 c2 c3 c4 c5 c6 =
  GithubRepo
    { githubRepoId :: c1
    , githubRepoName :: c2
    , githubRepoFullName :: c3
    , githubRepoDefaultBranch :: c4
    , githubRepoRemoteUri :: c5
    , githubRepoEnabled :: c6
    } deriving (Generic)

type GithubRepo = GithubRepo' GithubRepoId Text Text Text Text Bool

type GithubRepoReadColumns = GithubRepo' (Column PGInt4) (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGBool)

type GithubRepoWriteColumns = GithubRepo' (Maybe (Column PGInt4)) (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGBool)

type GithubRepoNullableColumns = GithubRepo' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGBool))

type GithubRepoNullable = GithubRepo' (Maybe GithubRepoId) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Bool)

fromNullableGithubRepo :: GithubRepoNullable -> Maybe GithubRepo
fromNullableGithubRepo = fromNullable

$(makeAdaptorAndInstance "pGithubRepo" ''GithubRepo')

githubRepoTable :: Table GithubRepoWriteColumns GithubRepoReadColumns
githubRepoTable = TableWithSchema "hercules" "github_repos" (pGithubRepo
  GithubRepo
    { githubRepoId = optional "id"
    , githubRepoName = required "name"
    , githubRepoFullName = required "full_name"
    , githubRepoDefaultBranch = required "default_branch"
    , githubRepoRemoteUri = required "remote_uri"
    , githubRepoEnabled = required "enabled"
    }
  )

---- Types for table: jobs ----

data Job' c1 c2 =
  Job
    { jobJobsetId :: c1
    , jobName :: c2
    }

type JobsetId = Int -- fixme: newtypes for ids

type Job = Job' JobsetId Text

type JobReadColumns = Job' (Column PGInt4) (Column PGText)

type JobWriteColumns = Job' (Column PGInt4) (Column PGText)

type JobNullableColumns = Job' (Column (Nullable PGInt4)) (Column (Nullable PGText))

type JobNullable = Job' (Maybe JobsetId) (Maybe Text)

fromNullableJob :: JobNullable -> Maybe Job
fromNullableJob = fromNullable

$(makeAdaptorAndInstance "pJob" ''Job')

jobTable :: Table JobWriteColumns JobReadColumns
jobTable = TableWithSchema "hercules" "jobs" (pJob
  Job
    { jobJobsetId = required "jobset_id"
    , jobName = required "name"
    }
  )

---- Types for table: jobsets ----

data Jobset' c1 c2 c3 c4 c5 c6 c7 =
  Jobset
    { jobsetBranchId :: c1
    , jobsetErrormsg :: c2
    , jobsetErrortime :: c3
    , jobsetLastcheckedtime :: c4
    , jobsetTriggertime :: c5
    , jobsetFetcherrormsg :: c6
    , jobsetStarttime :: c7
    } deriving (Generic)

type Jobset = Jobset' JobsetId (Maybe Text) (Maybe UTCTime) (Maybe UTCTime) (Maybe UTCTime) (Maybe Text) (Maybe UTCTime)

type JobsetReadColumns = Jobset' (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGTimestamptz)) (Column (Nullable PGTimestamptz)) (Column (Nullable PGTimestamptz)) (Column (Nullable PGText)) (Column (Nullable PGTimestamptz))

type JobsetWriteColumns = Jobset' (Column PGInt4) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGTimestamptz))) (Maybe (Column (Nullable PGTimestamptz))) (Maybe (Column (Nullable PGTimestamptz))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGTimestamptz)))

type JobsetNullableColumns = Jobset' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGTimestamptz)) (Column (Nullable PGTimestamptz)) (Column (Nullable PGTimestamptz)) (Column (Nullable PGText)) (Column (Nullable PGTimestamptz))

type JobsetNullable = Jobset' (Maybe JobsetId) (Maybe Text) (Maybe UTCTime) (Maybe UTCTime) (Maybe UTCTime) (Maybe Text) (Maybe UTCTime)

fromNullableJobset :: JobsetNullable -> Maybe Jobset
fromNullableJobset = fromNullable

$(makeAdaptorAndInstance "pJobset" ''Jobset')

jobsetTable :: Table JobsetWriteColumns JobsetReadColumns
jobsetTable = TableWithSchema "hercules" "jobsets" (pJobset
  Jobset
    { jobsetBranchId = required "branch_id"
    , jobsetErrormsg = optional "errormsg"
    , jobsetErrortime = optional "errortime"
    , jobsetLastcheckedtime = optional "lastcheckedtime"
    , jobsetTriggertime = optional "triggertime"
    , jobsetFetcherrormsg = optional "fetcherrormsg"
    , jobsetStarttime = optional "starttime"
    }
  )

instance ToJSON Jobset where
instance ElmType Jobset where

---- Types for table: sources ----

data Source' c1 c2 c3 =
  Source
    { sourceBranchId :: c1
    , sourcePath :: c2
    , sourceSha256 :: c3
    }

type Source = Source' Int32 Text Text

type SourceReadColumns = Source' (Column PGInt4) (Column PGText) (Column PGText)

type SourceWriteColumns = Source' (Column PGInt4) (Column PGText) (Column PGText)

type SourceNullableColumns = Source' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type SourceNullable = Source' (Maybe Int32) (Maybe Text) (Maybe Text)

fromNullableSource :: SourceNullable -> Maybe Source
fromNullableSource = fromNullable

$(makeAdaptorAndInstance "pSource" ''Source')

sourceTable :: Table SourceWriteColumns SourceReadColumns
sourceTable = TableWithSchema "hercules" "sources" (pSource
  Source
    { sourceBranchId = required "branch_id"
    , sourcePath = required "path"
    , sourceSha256 = required "sha256"
    }
  )

---- Types for table: jobsetevals ----

data Jobseteval' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 =
  Jobseteval
    { jobsetevalId           :: c1
    , jobsetevalProject      :: c2
    , jobsetevalJobsetId     :: c3
    , jobsetevalTimestamp    :: c4
    , jobsetevalCheckouttime :: c5
    , jobsetevalEvaltime     :: c6
    , jobsetevalHasnewbuilds :: c7
    , jobsetevalHash         :: c8
    , jobsetevalNrbuilds     :: c9
    , jobsetevalNrsucceeded  :: c10
    }

type JobsetevalId = Int

type Jobseteval = Jobseteval' JobsetevalId Text Int Int Int Int Int Text (Maybe Int) (Maybe Int)

type JobsetevalReadColumns = Jobseteval' (Column PGInt4) (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGText) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type JobsetevalWriteColumns = Jobseteval' (Maybe (Column PGInt4)) (Column PGText) (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGText) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4)))

type JobsetevalNullableColumns = Jobseteval' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type JobsetevalNullable = Jobseteval' (Maybe JobsetevalId) (Maybe Text) (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Text) (Maybe Int) (Maybe Int)

fromNullableJobseteval :: JobsetevalNullable -> Maybe Jobseteval
fromNullableJobseteval = fromNullable

$(makeAdaptorAndInstance "pJobseteval" ''Jobseteval')

jobsetevalTable :: Table JobsetevalWriteColumns JobsetevalReadColumns
jobsetevalTable = Table "jobsetevals" (pJobseteval
  Jobseteval
    { jobsetevalId = optional "id"
    , jobsetevalProject = required "project"
    , jobsetevalJobsetId = required "jobset_id"
    , jobsetevalTimestamp = required "timestamp"
    , jobsetevalCheckouttime = required "checkouttime"
    , jobsetevalEvaltime = required "evaltime"
    , jobsetevalHasnewbuilds = required "hasnewbuilds"
    , jobsetevalHash = required "hash"
    , jobsetevalNrbuilds = optional "nrbuilds"
    , jobsetevalNrsucceeded = optional "nrsucceeded"
    }
  )

---- Types for table: jobsetevalinputs ----

data Jobsetevalinput' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 =
  Jobsetevalinput
    { jobsetevalinputEval       :: c1
    , jobsetevalinputName       :: c2
    , jobsetevalinputAltnr      :: c3
    , jobsetevalinputType       :: c4
    , jobsetevalinputUri        :: c5
    , jobsetevalinputRevision   :: c6
    , jobsetevalinputValue      :: c7
    , jobsetevalinputDependency :: c8
    , jobsetevalinputPath       :: c9
    , jobsetevalinputSha256Hash :: c10
    }

type Jobsetevalinput = Jobsetevalinput' JobsetevalId Text Int Text (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int) (Maybe Text) (Maybe Text)

type JobsetevalinputReadColumns = Jobsetevalinput' (Column PGInt4) (Column PGText) (Column PGInt4) (Column PGText) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type JobsetevalinputWriteColumns = Jobsetevalinput' (Column PGInt4) (Column PGText) (Column PGInt4) (Column PGText) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText)))

type JobsetevalinputNullableColumns = Jobsetevalinput' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type JobsetevalinputNullable = Jobsetevalinput' (Maybe JobsetevalId) (Maybe Text) (Maybe Int) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int) (Maybe Text) (Maybe Text)

fromNullableJobsetevalinput :: JobsetevalinputNullable -> Maybe Jobsetevalinput
fromNullableJobsetevalinput = fromNullable

$(makeAdaptorAndInstance "pJobsetevalinput" ''Jobsetevalinput')

jobsetevalinputTable :: Table JobsetevalinputWriteColumns JobsetevalinputReadColumns
jobsetevalinputTable = Table "jobsetevalinputs" (pJobsetevalinput
  Jobsetevalinput
    { jobsetevalinputEval = required "eval"
    , jobsetevalinputName = required "name"
    , jobsetevalinputAltnr = required "altnr"
    , jobsetevalinputType = required "type"
    , jobsetevalinputUri = optional "uri"
    , jobsetevalinputRevision = optional "revision"
    , jobsetevalinputValue = optional "value"
    , jobsetevalinputDependency = optional "dependency"
    , jobsetevalinputPath = optional "path"
    , jobsetevalinputSha256Hash = optional "sha256hash"
    }
  )

---- Types for table: jobsetevalmembers ----

data Jobsetevalmember' c1 c2 c3 =
  Jobsetevalmember
    { jobsetevalmemberEval  :: c1
    , jobsetevalmemberBuild :: c2
    , jobsetevalmemberIsnew :: c3
    }

type Jobsetevalmember = Jobsetevalmember' Int32 Int32 Int32

type JobsetevalmemberReadColumns = Jobsetevalmember' (Column PGInt4) (Column PGInt4) (Column PGInt4)

type JobsetevalmemberWriteColumns = Jobsetevalmember' (Column PGInt4) (Column PGInt4) (Column PGInt4)

type JobsetevalmemberNullableColumns = Jobsetevalmember' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type JobsetevalmemberNullable = Jobsetevalmember' (Maybe Int32) (Maybe Int32) (Maybe Int32)

fromNullableJobsetevalmember :: JobsetevalmemberNullable -> Maybe Jobsetevalmember
fromNullableJobsetevalmember = fromNullable

$(makeAdaptorAndInstance "pJobsetevalmember" ''Jobsetevalmember')

jobsetevalmemberTable :: Table JobsetevalmemberWriteColumns JobsetevalmemberReadColumns
jobsetevalmemberTable = Table "jobsetevalmembers" (pJobsetevalmember
  Jobsetevalmember
    { jobsetevalmemberEval = required "eval"
    , jobsetevalmemberBuild = required "build"
    , jobsetevalmemberIsnew = required "isnew"
    }
  )

---- Types for table: jobsetinputalts ----

data Jobsetinputalt' c1 c2 c3 c4 c5 c6 =
  Jobsetinputalt
    { jobsetinputaltProject  :: c1
    , jobsetinputaltJobset   :: c2
    , jobsetinputaltInput    :: c3
    , jobsetinputaltAltnr    :: c4
    , jobsetinputaltValue    :: c5
    , jobsetinputaltRevision :: c6
    }

type Jobsetinputalt = Jobsetinputalt' Text Text Text Int32 (Maybe Text) (Maybe Text)

type JobsetinputaltReadColumns = Jobsetinputalt' (Column PGText) (Column PGText) (Column PGText) (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGText))

type JobsetinputaltWriteColumns = Jobsetinputalt' (Column PGText) (Column PGText) (Column PGText) (Column PGInt4) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText)))

type JobsetinputaltNullableColumns = Jobsetinputalt' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type JobsetinputaltNullable = Jobsetinputalt' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Text) (Maybe Text)

fromNullableJobsetinputalt :: JobsetinputaltNullable -> Maybe Jobsetinputalt
fromNullableJobsetinputalt = fromNullable

$(makeAdaptorAndInstance "pJobsetinputalt" ''Jobsetinputalt')

jobsetinputaltTable :: Table JobsetinputaltWriteColumns JobsetinputaltReadColumns
jobsetinputaltTable = Table "jobsetinputalts" (pJobsetinputalt
  Jobsetinputalt
    { jobsetinputaltProject = required "project"
    , jobsetinputaltJobset = required "jobset"
    , jobsetinputaltInput = required "input"
    , jobsetinputaltAltnr = required "altnr"
    , jobsetinputaltValue = optional "value"
    , jobsetinputaltRevision = optional "revision"
    }
  )

---- Types for table: jobsetinputs ----

data Jobsetinput' c1 c2 c3 c4 c5 =
  Jobsetinput
    { jobsetinputProject          :: c1
    , jobsetinputJobset           :: c2
    , jobsetinputName             :: c3
    , jobsetinputType             :: c4
    , jobsetinputEmailresponsible :: c5
    }

type Jobsetinput = Jobsetinput' Text Text Text Text Int32

type JobsetinputReadColumns = Jobsetinput' (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGInt4)

type JobsetinputWriteColumns = Jobsetinput' (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGInt4)

type JobsetinputNullableColumns = Jobsetinput' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4))

type JobsetinputNullable = Jobsetinput' (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Int32)

fromNullableJobsetinput :: JobsetinputNullable -> Maybe Jobsetinput
fromNullableJobsetinput = fromNullable

$(makeAdaptorAndInstance "pJobsetinput" ''Jobsetinput')

jobsetinputTable :: Table JobsetinputWriteColumns JobsetinputReadColumns
jobsetinputTable = Table "jobsetinputs" (pJobsetinput
  Jobsetinput
    { jobsetinputProject = required "project"
    , jobsetinputJobset = required "jobset"
    , jobsetinputName = required "name"
    , jobsetinputType = required "type"
    , jobsetinputEmailresponsible = required "emailresponsible"
    }
  )

---- Types for table: jobsetrenames ----

data Jobsetrename' c1 c2 c3 =
  Jobsetrename
    { jobsetrenameProject :: c1
    , jobsetrenameFrom    :: c2
    , jobsetrenameTo      :: c3
    }

type Jobsetrename = Jobsetrename' Text Text Text

type JobsetrenameReadColumns = Jobsetrename' (Column PGText) (Column PGText) (Column PGText)

type JobsetrenameWriteColumns = Jobsetrename' (Column PGText) (Column PGText) (Column PGText)

type JobsetrenameNullableColumns = Jobsetrename' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type JobsetrenameNullable = Jobsetrename' (Maybe Text) (Maybe Text) (Maybe Text)

fromNullableJobsetrename :: JobsetrenameNullable -> Maybe Jobsetrename
fromNullableJobsetrename = fromNullable

$(makeAdaptorAndInstance "pJobsetrename" ''Jobsetrename')

jobsetrenameTable :: Table JobsetrenameWriteColumns JobsetrenameReadColumns
jobsetrenameTable = Table "jobsetrenames" (pJobsetrename
  Jobsetrename
    { jobsetrenameProject = required "project"
    , jobsetrenameFrom = required "from_"
    , jobsetrenameTo = required "to_"
    }
  )

---- Types for table: github_app ----

data GithubApp' c1 c2 =
  GithubApp
    { githubAppAppId :: c1
    , githubAppUpdatedAt :: c2
    } deriving Generic

type GithubApp = GithubApp' Int UTCTime

type GithubAppReadColumns = GithubApp' (Column PGInt4) (Column PGTimestamptz)

type GithubAppWriteColumns = GithubApp' (Column PGInt4) (Column PGTimestamptz)

type GithubAppNullableColumns = GithubApp' (Column (Nullable PGInt4)) (Column (Nullable PGTimestamptz))

type GithubAppNullable = GithubApp' (Maybe Int) (Maybe UTCTime)

fromNullableGithubApp :: GithubAppNullable -> Maybe GithubApp
fromNullableGithubApp = fromNullable

$(makeAdaptorAndInstance "pGithubApp" ''GithubApp')

githubAppTable :: Table GithubAppWriteColumns GithubAppReadColumns
githubAppTable = TableWithSchema "hercules" "github_app" (pGithubApp
  GithubApp
    { githubAppAppId = required "app_id"
    , githubAppUpdatedAt = required "updated_at"
    }
  )

instance ToJSON GithubApp where
instance ElmType GithubApp where

----------------------------------------------------------------------------
-- not autogenerated ...

-- fixme: implement this boilerplate in opaleye-gen
pgJobseteval :: Jobseteval -> JobsetevalWriteColumns
pgJobseteval = pJobseteval Jobseteval
    { jobsetevalId           = const Nothing
    , jobsetevalProject      = pgStrictText
    , jobsetevalJobsetId     = pgInt4
    , jobsetevalTimestamp    = pgInt4
    , jobsetevalCheckouttime = pgInt4
    , jobsetevalEvaltime     = pgInt4
    , jobsetevalHasnewbuilds = pgInt4
    , jobsetevalHash         = pgStrictText
    , jobsetevalNrbuilds     = fmap (toNullable . fromIntegral)
    , jobsetevalNrsucceeded  = fmap (toNullable . fromIntegral)
    }

instance Default Constant Jobseteval JobsetevalWriteColumns where
   def = Constant pgJobseteval

pgJobsetevalinput :: Jobsetevalinput -> JobsetevalinputWriteColumns
pgJobsetevalinput = pJobsetevalinput Jobsetevalinput
    { jobsetevalinputEval       = constant
    , jobsetevalinputName       = pgStrictText
    , jobsetevalinputAltnr      = constant
    , jobsetevalinputType       = pgStrictText
    , jobsetevalinputUri        = pgNullableText
    , jobsetevalinputRevision   = pgNullableText
    , jobsetevalinputValue      = pgNullableText
    , jobsetevalinputDependency = fmap (toNullable . fromIntegral)
    , jobsetevalinputPath       = pgNullableText
    , jobsetevalinputSha256Hash = pgNullableText
    }

instance Default Constant Jobsetevalinput JobsetevalinputWriteColumns where
   def = Constant pgJobsetevalinput

pgNullableText :: Maybe Text -> Maybe (Column (Nullable PGText))
pgNullableText = fmap (toNullable . pgStrictText)

pgNullableTimestamp :: Maybe UTCTime -> Maybe (Column (Nullable PGTimestamptz))
pgNullableTimestamp = fmap (toNullable . pgUTCTime)

pgGithubRepo :: GithubRepo -> GithubRepoWriteColumns
pgGithubRepo = pGithubRepo GithubRepo
  { githubRepoId = const Nothing
  , githubRepoName = pgStrictText
  , githubRepoFullName = pgStrictText
  , githubRepoDefaultBranch = pgStrictText
  , githubRepoRemoteUri = pgStrictText
  , githubRepoEnabled = pgBool
  }

pgGithubBranch :: GithubBranch -> GithubBranchWriteColumns
pgGithubBranch = pGithubBranch GithubBranch
  { githubBranchId = const Nothing
  , githubBranchRepoId = pgInt4
  , githubBranchName = pgStrictText
  , githubBranchRev = pgStrictText
  , githubBranchSpec = pgValueJSONB
  , githubBranchPullRequestNumber = fmap (toNullable . pgInt4)
  }

pgGithubApp :: GithubApp -> GithubAppWriteColumns
pgGithubApp = pGithubApp GithubApp
  { githubAppAppId = pgInt4
  , githubAppUpdatedAt = pgUTCTime
  }

pgGithubPullRequest :: GithubPullRequest -> GithubPullRequestWriteColumns
pgGithubPullRequest = pGithubPullRequest GithubPullRequest
  { githubPullRequestRepoId = pgInt4
  , githubPullRequestNumber = pgInt4
  , githubPullRequestTitle = pgStrictText
  }

githubPullRequestPK :: GithubPullRequest' n r t -> (r, n)
githubPullRequestPK pr = (githubPullRequestRepoId pr, githubPullRequestNumber pr)

pgGithubRepoCache :: GithubRepoCache -> GithubRepoCacheWriteColumns
pgGithubRepoCache = pGithubRepoCache GithubRepoCache
  { githubRepoCacheRepoId = pgInt4
  , githubRepoCachePath = pgStrictText
  , githubRepoCacheStartFetch = pgNullableTimestamp
  , githubRepoCacheLastFetch = pgNullableTimestamp
  }

pgJobset :: Jobset -> JobsetWriteColumns
pgJobset = pJobset Jobset
  { jobsetBranchId = pgInt4
  , jobsetErrormsg = pgNullableText
  , jobsetErrortime = pgNullableTimestamp
  , jobsetLastcheckedtime = pgNullableTimestamp
  , jobsetTriggertime = pgNullableTimestamp
  , jobsetFetcherrormsg = pgNullableText
  , jobsetStarttime = pgNullableTimestamp
  }
