{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Hercules.Evaluate.Jobset
  ( checkJobset
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception.Lifted
import Control.Monad.IO.Class
import Safe
import Say
import Data.Monoid
import Control.Monad
import Data.Maybe
import Data.List (nubBy, sort)
import Data.Time.Clock (NominalDiffTime, getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Prelude hiding (null)
import Hercules.ServerEnv
import Hercules.Evaluate.Types
import Hercules.Input.Git
import Hercules.Evaluate.Spec
import Hercules.Query.Hercules
import Hercules.Database.Hercules
import GHC.Int
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Opaleye
import System.Random.Shuffle (shuffleM)
import System.FilePath ((</>))
import Data.Ord (comparing)

import Hercules.Evaluate.ReleaseExpr
import Hercules.Evaluate.Fetch
import qualified Hercules.Database.Hydra as Hydra
import Hercules.Query.Hydra

data JobsetInfo = JobsetInfo { infoRepo :: GithubRepo
                             , infoBranch :: GithubBranch
                             , infoSpec :: BuildSpec
                             }

-- | Jobset ID -- happens to also be the branch id currently.
jobsetInfoId :: JobsetInfo -> JobsetId
jobsetInfoId = githubBranchId . infoBranch

-- | Project name text for a jobset.
jobsetInfoProject :: JobsetInfo -> Text
jobsetInfoProject = githubRepoName . infoRepo

jobsetInfoKey :: JobsetInfo -> Text
jobsetInfoKey js = jobsetInfoProject js <> ":" <> jobsetName js
  where jobsetName = githubBranchName . infoBranch

evaluateJobset :: JobsetInfo -> App ()
evaluateJobset js@(JobsetInfo repo branch spec) = do
  ((src, buildInputs), checkoutTime) <- timeAction $ do
    src <- fetchJobsetSource repo branch
    buildInputs <- fetchInputs (githubRepoName repo) (githubBranchName branch) (buildInputs spec)
    return (src, buildInputs)

  let exprType = pathExprLang (buildJobset (srcBuildSpec src))
      nixExprFullPath = srcNixStorePath src </> buildJobset spec
      argsHash = hashReleaseExpr exprType nixExprFullPath buildInputs

  -- If prev eval had exact same inputs, bail out.
  prev <- checkUnchanged (jobsetInfoId js) argsHash
  when (isNothing prev) $ do
    (eval, evalTime) <- timeAction . liftIO $ evalJobs exprType buildInputs nixExprFullPath

    (_, dbTime) <- withHerculesConnection (timeAction . createBuilds js eval src buildInputs argsHash checkoutTime evalTime)

    -- Store the error messages for jobs that failed to evaluate.
    updateJobsetError (evalErrorMsgs eval) (jobsetInfoId js)

branchBuildSpec :: GithubBranch -> Maybe BuildSpec
branchBuildSpec = parseMaybe parseJSON . githubBranchSpec

timeAction :: MonadIO m => m a -> m (a, NominalDiffTime)
timeAction act = do
  start <- liftIO getPOSIXTime
  r <- act
  stop <- liftIO getPOSIXTime
  return (r, stop - start)

-- | corresponds to perl function evalJobs
evalJobs :: ReleaseExprLang -> BuildInputs -> FilePath -> IO EvalResult
evalJobs lang inputInfo nixExprPath = do
  let gcRootsDir = Nothing -- fixme: config environment
  res <- evaluateReleaseExpr lang gcRootsDir nixExprPath inputInfo
  case res of
    Left err -> fail err
    Right jobs | any T.null (M.keys jobs) -> return jobs
               | otherwise -> fail "Jobset contains a job with an empty name. Make sure the jobset evaluates to an attrset of jobs."

-- | The list of (job name, build info) pairs to build.
buildSchedule :: EvalResult -> IO [(Text, JobEvalResult)]
buildSchedule jobs = shuffleM $ uniq [(name, job) | (name, Right job) <- M.assocs jobs]
  where
    uniq = nubBy (\a b -> nfo a == nfo b) -- fixme: n^2
    nfo (name, job) = (name, snd <$> firstOutput (evalResultOutputs job))

hydraTimestamp :: POSIXTime -> Int
hydraTimestamp = fromInteger . floor

createBuilds :: JobsetInfo -> EvalResult -> JobsetSource -> BuildInputs -> Text -> NominalDiffTime -> NominalDiffTime -> Connection -> IO ()
createBuilds js jobs src buildInputs argsHash checkoutTime evalTime conn = withTransaction conn $ do
  prev <- getPrevJobsetEval True (jobsetInfoId js) conn
  prevSize <- case prev of
    Just eval -> fmap fromIntegral . headMay <$> (runQuery conn (countEvalMembers eval) :: IO [Int64])
    Nothing -> return Nothing

  -- Clear the "current" flag on all builds.  Since we're in a
  -- transaction this will only become visible after the new
  -- current builds have been added.
  setBuildsCurrent False js conn

  -- Schedule each successfully evaluated job.
  jobs' <- buildSchedule jobs
  results <- mapM (checkBuild conn js src prev) jobs'
  now <- getPOSIXTime

  let jobsetChanged = any (\(_, (new, _, _)) -> new) results || maybe True (/= length results) prevSize
      ev = Jobseteval
           { jobsetevalId           = 0
           , jobsetevalProject      = githubRepoName (infoRepo js)
           , jobsetevalJobsetId     = fromIntegral $ jobsetInfoId js
           , jobsetevalTimestamp    = hydraTimestamp now
           , jobsetevalCheckouttime = hydraTimestamp checkoutTime
           , jobsetevalEvaltime     = hydraTimestamp evalTime
           , jobsetevalHasnewbuilds = if jobsetChanged then 1 else 0
           , jobsetevalHash         = argsHash
           , jobsetevalNrbuilds     = if jobsetChanged then Just (fromIntegral (length results)) else Nothing
           , jobsetevalNrsucceeded  = Nothing
           }

  [evId] <- runInsertManyReturning conn jobsetevalTable [pgJobseteval ev] jobsetevalId :: IO [Int32]

  if jobsetChanged
    then do
      let
        members = [ Jobsetevalmember evId buildId (if new then 1 else 0 :: Int)
                  | (buildId, (new, _, _)) <- results ]
        -- fixme: figure out where this input info comes from exactly
        -- fixme: hydra-eval-jobset supports multiple names
        inputs = [ Jobsetevalinput
                   { jobsetevalinputEval       = evId
                   , jobsetevalinputName       = name
                   , jobsetevalinputAltnr      = 0
                   , jobsetevalinputType       = inputSpecType . buildInputSpec $ input
                   , jobsetevalinputUri        = Just . T.pack . show . inputSpecUri . buildInputSpec $ input
                   , jobsetevalinputRevision   = buildInputRev input
                   , jobsetevalinputValue      = Just . inputSpecValue . buildInputSpec $ input
                   , jobsetevalinputDependency = buildInputBuildId input
                   , jobsetevalinputPath       = T.pack <$> buildInputStorePath input
                   , jobsetevalinputSha256Hash = buildInputHash input
                   } | (name, input) <- M.assocs buildInputs ]

        -- Create AggregateConstituents mappings.  Since there can
        -- be jobs that alias each other, if there are multiple
        -- builds for the same derivation, pick the one with the
        -- shortest name.
        -- fixme: untangle the perl and insert aggregateconstituents rows
        -- ag = [ Hydra.Aggregateconstituent a c
        --      | (buildId, (_, _, _)) <- results ]
        ag = []

      void $ runInsertMany conn jobsetevalmemberTable (map constant members)
      void $ runInsertMany conn Hydra.aggregateconstituentTable ag
      void $ runInsertMany conn jobsetevalinputTable (map pgJobsetevalinput inputs)


      sayErrString $ "  Created new eval " <> show evId
      setBuildsCurrent True js conn -- fixme: check if same as $ev->builds->update({iscurrent => 1});

    else do
      sayErrString $ "  created cached eval " <> show evId
      case prev of
        -- fixme: this is probably not updating the correct build
        Just eval -> setBuildsCurrent True js conn
        Nothing -> return ()

  -- wake up hydra-queue-runner
  case minimumMay [id | (id, (True, _, _)) <- results] of
    Just lowestId -> return () -- someTrigger "notifyAdded" lowestId
    Nothing -> return ()

  updateJobsetCheckTime (jobsetInfoId js) conn

type PathMap = Map Text FilePath -- fixme: not sure what it is yet

firstOutput :: Map Text Text -> Maybe (Text, Text)
firstOutput outputs = minimumByMay (comparing fst) (M.assocs outputs)

-- | This is called checkBuild in perl but it actually inserts rows.
-- Check whether to add the build described by $buildInfo.
checkBuild :: Connection -> JobsetInfo -> JobsetSource -> Maybe Jobseteval
           -> (AttrPath, JobEvalResult)
           -> IO (Int, (Bool, Text, FilePath))
checkBuild conn js src prevEval (jobName, buildInfo) = do
  putStrLn $ "considering job $project->name:" <> T.unpack (jobsetInfoKey js)

  -- In various checks we can use an arbitrary output (the first)
  -- rather than all outputs, since if one output is the same, the
  -- others will be as well.
  case firstOutput (evalResultOutputs buildInfo) of
    Just (firstOutputName, firstOutputPath) -> withTransaction conn $ do
      job <- getOrCreateJob conn js jobName

      -- Don't add a build that has already been scheduled for this
      -- job, or has been built but is still a "current" build for
      -- this job.  Note that this means that if the sources of a job
      -- are changed from A to B and then reverted to A, three builds
      -- will be performed (though the last one will probably use the
      -- cached result from the first).  This ensures that the builds
      -- with the highest ID will always be the ones that we want in
      -- the channels.  FIXME: Checking the output paths doesn't take
      -- meta-attributes into account.  For instance, do we want a
      -- new build to be scheduled if the meta.maintainers field is
      -- changed?
      prevBuildId <- case prevEval of
        Just eval -> do
          -- let jobsetName = "$jobset->name"
          -- let projectName = "$jobset->project->name"
          listToMaybe <$> runQuery conn (prevBuildIdQuery jobName eval firstOutputName firstOutputPath)
        Nothing -> return Nothing

      case prevBuildId of
        Just buildId -> return (buildId, (False, jobName, evalResultDrvPath buildInfo))
        Nothing -> do
          now <- getPOSIXTime
          let build = makeBuild jobName now buildInfo src js
          [buildId] <- runInsertManyReturning conn Hydra.buildTable [Hydra.pgBuild build] Hydra.buildId
          let buildOutputs = [ Hydra.Buildoutput buildId name (evalResultOutputs buildInfo ! name)
                             | name <- sort (M.keys (evalResultOutputs buildInfo)) ]
          _ <- runInsertMany conn Hydra.buildoutputTable (map constant buildOutputs)
          return (buildId, (True, jobName, evalResultDrvPath buildInfo))

    Nothing -> fail "job has no outputs"

getOrCreateJob :: Connection -> JobsetInfo -> AttrPath -> IO ()
getOrCreateJob conn js name = withTransaction conn $ do
  let job = Job (jobsetInfoId js) name
  [exists] <- runQuery conn (jobExists job) :: IO [Int64]
  when (exists == 0) $ void $ runInsertMany conn jobTable [constant job]

-- fixme: how does this link to eval?
makeBuild :: Text -> POSIXTime -> JobEvalResult -> JobsetSource -> JobsetInfo -> Hydra.Build
makeBuild jobName time JobEvalResult{..} (JobsetSource nixExprPath _ spec) js = Hydra.Build
  { buildId             = 0
  , buildFinished       = 0
  , buildTimestamp      = fromIntegral $ hydraTimestamp time
  , buildProject        = jobsetInfoProject js
  , buildJobset         = githubBranchName (infoBranch js)
  , buildJob            = jobName
  , buildNixname        = Just evalResultNixName
  , buildDescription    = Just evalResultDescription
  , buildDrvpath        = T.pack evalResultDrvPath
  , buildSystem         = evalResultSystem
  , buildLicense        = buildItemList $ evalResultLicense
  , buildHomepage       = buildItemList $ evalResultHomepage
  , buildMaintainers    = buildItemList $ evalResultMaintainers
  , buildMaxsilent      = Just . fromIntegral $ evalResultMaxSilent
  , buildTimeout        = Just . fromIntegral $ evalResultTimeout
  , buildIschannel      = if evalResultIsChannel then 1 else 0
  , buildIscurrent      = Just 1
  , buildNixexprinput   = Just "fixme is the input name required?"
  , buildNixexprpath    = Just . T.pack $ nixExprPath
  , buildPriority       = fromIntegral evalResultSchedulingPriority
  , buildGlobalpriority = 0
  , buildStarttime      = Nothing
  , buildStoptime       = Nothing
  , buildIscachedbuild  = Just 0
  , buildBuildstatus    = Nothing
  , buildSize           = Nothing
  , buildClosuresize    = Nothing
  , buildReleasename    = Nothing
  , buildKeep           = 3
  }

buildItemList :: [Text] -> Maybe Text
buildItemList [] = Nothing
buildItemList xs = Just (T.intercalate "," xs)

setBuildsCurrent :: Bool -> JobsetInfo -> Connection -> IO ()
setBuildsCurrent current js c = void $ runUpdateEasy c Hydra.buildTable update pred
  where
    update b = b { Hydra.buildIscurrent = toNullable . pgInt4 $ v }
    pred b = Hydra.buildJobset b .== pgStrictText (githubBranchName (infoBranch js))
             .&& Hydra.buildProject b .== pgStrictText (jobsetInfoProject js)
             .&& Hydra.buildIscurrent b .== toNullable (pgInt4 1)
    v = if current then 1 else 0

data JobsetSource = JobsetSource
  { srcNixStorePath :: NixStorePath
  , srcNixStoreHash :: NixStoreHash
  , srcBuildSpec    :: BuildSpec
  } deriving (Show, Eq)

fetchJobsetSource :: GithubRepo -> GithubBranch -> App JobsetSource
fetchJobsetSource repo branch = undefined

-- | Find previous evaluation and its number of builds
getPrevJobsetEval :: Bool -> JobsetId -> Connection -> IO (Maybe Jobseteval)
getPrevJobsetEval hasNewBuilds js conn = listToMaybe <$> runQuery conn (prevJobsetEvalQuery hasNewBuilds js)

-- Hash the arguments to hydra-eval-jobs and check the
-- JobsetInputHashes to see if the previous evaluation had the same
-- inputs.
type Evaluation  = Jobseteval
checkUnchanged :: JobsetId -> Text -> App (Maybe Evaluation)
checkUnchanged js argsHash = do
  prev <- withHerculesConnection (getPrevJobsetEval False js)
  return $ if isUnchanged prev then prev else Nothing
  where
    isUnchanged = maybe False ((== argsHash) . jobsetevalHash)

checkJobset :: ProjectName -> JobsetName -> App Bool
checkJobset p j = do
  res <- withHerculesConnection $ lookupBranch p j
  case res of
    Just (repo, branch) -> do
      case jobsetInfo repo branch of
        Just js -> checkJobset' js
        Nothing -> do
          sayErr $ "specified jobset \"" <> p <> ":" <> j <> "\" is not enabled"
          return False
    Nothing -> do
      sayErr $ "specified jobset \"" <> p <> ":" <> j <> "\" does not exist"
      return False

checkJobset' :: JobsetInfo -> App Bool
checkJobset' js = do
  jsId <- withHerculesConnection $ getOrCreateJobset js
  res <- try (evaluateJobset js)
  case res of
    Left (err :: IOException) -> do
      sayErrString $ "problem evaluating jobset " ++ show err
      updateJobsetError (T.pack $ show err) (jobsetInfoId js)
      return False
    Right ev -> do
      withHerculesConnection $ updateJobsetCheckTime jsId
      return True

findJobsetOld :: ProjectName -> JobsetName -> App (Maybe (Jobset, GithubBranch, GithubRepo))
findJobsetOld p j = runHerculesQueryWithConnectionSingular (findJobsetQueryOld p j)

-- | Jobset info for a branch.
-- Returns Nothing if disabled or spec missing.
jobsetInfo :: GithubRepo -> GithubBranch -> Maybe JobsetInfo
jobsetInfo repo branch = JobsetInfo repo branch <$> spec
  where spec | githubRepoEnabled repo = branchBuildSpec branch
             | otherwise = Nothing

lookupBranch :: ProjectName -> JobsetName -> Connection -> IO (Maybe (GithubRepo, GithubBranch))
lookupBranch p j conn = listToMaybe <$> runQuery conn (findJobsetQuery p j)

getOrCreateJobset :: JobsetInfo -> Connection -> IO JobsetId
getOrCreateJobset js conn = withTransaction conn $ do
  let jsId = jobsetInfoId js
      jobset = Jobset (pgInt4 (fromIntegral jsId)) Nothing Nothing Nothing Nothing Nothing Nothing :: JobsetWriteColumns
  [prev] <- runQuery conn (countRows $ jobsetByIdQuery (fromIntegral jsId)) :: IO [Int64]
  when (prev == 0) . void $
    runInsertMany conn jobsetTable [jobset]
  return jsId

updateJobsetError :: Text -> JobsetId -> App ()
updateJobsetError err js = do
  now <- liftIO getCurrentTime
  void $ runUpdateWithConnection jobsetTable (updates now) (jobsetRestriction js)
  where
    -- fixme: looks rather awful, also overwrites trigger time and start time
    updates now _ = Jobset (pgInt4 (fromIntegral js))
                    (Just . toNullable . pgStrictText $ err) -- error message
                    (Just . toNullable $ pgUTCTime now) -- error time
                    (Just . toNullable $ pgUTCTime now) -- last checked time
                    Nothing     -- trigger time
                    (Just null) -- fetch error message
                    Nothing     -- start time

updateJobsetCheckTime :: JobsetId -> Connection -> IO ()
updateJobsetCheckTime js conn = do
  now <- getCurrentTime
  void $ runUpdate conn jobsetTable (updates now) (jobsetRestriction js)
  where
    updates now _ = Jobset (pgInt4 (fromIntegral js))
                    Nothing Nothing
                    (Just . toNullable $ pgUTCTime now)
                    Nothing (Just null) Nothing

fetchInputs :: Text -> Text -> Map Text InputSpec -> App BuildInputs
fetchInputs p j inputs = M.fromList . (zip (M.keys inputs)) <$> rs inputs
  where
    rs :: Map Text InputSpec -> App [BuildInput]
    rs = mapM (fetchInput p j) . M.elems

fetchInput :: Text -> Text -> InputSpec -> App BuildInput
fetchInput _ _ s@(InputSpecGit uri depth branch) = do
  res <- liftIO $ fetchInputGit uri depth branch
  return $ BuildInputGit s (fetchGitStorePath res) (fetchGitSHA256Hash res) M.empty -- fixme: include full fetchinputgit result
fetchInput _ _ s@(InputSpecNix _) = return $ BuildInputValue s
fetchInput _ _ s@(InputSpecBoolean _) = return $ BuildInputBoolean s
fetchInput _ _ s@(InputSpecString _) = return $ BuildInputString s
fetchInput _ _ s@(InputSpecValue _) = return $ BuildInputValue s
fetchInput p j s@(InputSpecPreviousBuild _) = fetchInputBuild s p j
fetchInput _ _ s@(InputSpecPreviousEvaluation v) = fetchInputEvaluation s v

-- fixme: implement fetchInputBuild
fetchInputBuild :: InputSpec -> Text -> Text -> App BuildInput
fetchInputBuild i projectName jobsetName = return $ BuildInputPreviousBuild i "" "" 0

-- fixme: implement fetchInputEvaluation
fetchInputEvaluation :: InputSpec -> Text -> App BuildInput
fetchInputEvaluation i value = return $ BuildInputPreviousEvaluation i M.empty

-- other things to fix
-- TODO: decouple jobset/project from github branch/rep
