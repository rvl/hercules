{-# LANGUAGE OverloadedStrings #-}

module Hercules.Evaluate.Fetch
  ( BuildInput(..)
  , BuildInputs
  , NixStorePath
  , NixStoreHash
  , buildInputStorePath
  , buildInputHash
  , buildInputSpec
  , buildInputRev
  ) where

import Hercules.Evaluate.Spec
import Hercules.Evaluate.Types

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M

type BuildInputs = Map Text BuildInput

-- | A build input which has been fetched.
-- fixme: use gadt
data BuildInput = BuildInputGit InputSpec NixStorePath NixStoreHash (Map Text Text)
                | BuildInputNix InputSpec
                | BuildInputBoolean InputSpec
                | BuildInputString InputSpec
                | BuildInputValue InputSpec
                | BuildInputPreviousBuild InputSpec NixStorePath NixStoreHash
                | BuildInputPreviousEvaluation InputSpec (Map Text NixStorePath)
                deriving (Show, Eq)

buildInputSpec :: BuildInput -> InputSpec
buildInputSpec (BuildInputGit s _ _ _)            = s
buildInputSpec (BuildInputNix s)                  = s
buildInputSpec (BuildInputBoolean s)              = s
buildInputSpec (BuildInputString s)               = s
buildInputSpec (BuildInputValue s)                = s
buildInputSpec (BuildInputPreviousBuild s _ _)    = s
buildInputSpec (BuildInputPreviousEvaluation s _) = s

buildInputStorePath :: BuildInput -> Maybe NixStorePath
buildInputStorePath (BuildInputGit _ p _ _) = Just p
buildInputStorePath (BuildInputPreviousBuild _ p _) = Just p
buildInputStorePath _ = Nothing

buildInputHash :: BuildInput -> Maybe Text
buildInputHash (BuildInputGit _ _ h _)         = Just h
buildInputHash (BuildInputPreviousBuild _ _ h) = Just h
buildInputHash _                               = Nothing

buildInputRev :: BuildInput -> Maybe Text
buildInputRev (BuildInputGit _ _ _ a) = M.lookup "rev" a
buildInputRev _ = Nothing
