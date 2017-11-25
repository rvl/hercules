{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

{-|
A module to handle the different queries we might want to make to Hydra's
database
-}
module Hercules.Query.Hydra
  ( projectNameQuery
  , projectQuery
  , projectsQuery
  , projectsWithJobsetsQuery
  , prevBuildIdQuery
  , countEvalMembers
  ) where

import Control.Arrow (returnA)
import Data.Text
import Opaleye

import Hercules.Database.Hydra
import Hercules.Database.Hercules

-- | A query to get a list of all the project names
projectNameQuery :: Query (Column PGText)
projectNameQuery = proc () -> do
  Project{..} <- queryTable projectTable -< ()
  returnA -< projectName

-- | A query to get a list of all the projects
projectsQuery :: Query ProjectReadColumns
projectsQuery = queryTable projectTable

-- | A query to get all the projects with the specified name (There should be
-- only one)
projectQuery :: Text -> Query ProjectReadColumns
projectQuery name = proc () -> do
  project@Project{..} <- queryTable projectTable -< ()
  restrict -< projectName .== pgStrictText name
  returnA -< project

-- | A query to get a list of all the jobsets
jobsetsQuery :: Query JobsetReadColumns
jobsetsQuery = queryTable jobsetTable

projectsWithJobsetsQuery
  :: Query (ProjectReadColumns, JobsetNullableColumns)
projectsWithJobsetsQuery = leftJoin projectsQuery jobsetsQuery eqName
  where
    eqName (Project{..}, Jobset{..}) = projectName .== pgString "" -- fixme: jobsetProject

-- | Find a previous build by looking for a build output
prevBuildIdQuery :: Text -> Jobseteval -> Text -> Text -> Query (Column PGInt4)
prevBuildIdQuery jobName prevEval outputName outputPath = limit 1 $ proc () -> do
  b <- queryTable buildTable -< ()
  m <- queryTable jobsetevalmemberTable -< ()
  o <- queryTable buildoutputTable -< ()

  restrict -< jobsetevalmemberEval m .== constant (jobsetevalId prevEval)
  restrict -< buildJob b .== constant jobName
  restrict -< buildoutputName o .== pgStrictText outputName
  restrict -< buildoutputPath o .== pgStrictText outputPath

  -- hydra-eval-jobset.pl has note about extra constraints
  -- making query faster. todo: add column indices
  -- restrict -< Hydra.buildProject b .== projectName
  -- restrict -< Hydra.buildJobset b .== jobsetName

  returnA -< buildId b

countEvalMembers :: Jobseteval -> Query (Column PGInt8)
countEvalMembers eval = countRows $ proc () -> do
  m <- queryTable jobsetevalmemberTable -< ()
  restrict -< jobsetevalmemberEval m .== constant (jobsetevalId eval)
  returnA -< m
