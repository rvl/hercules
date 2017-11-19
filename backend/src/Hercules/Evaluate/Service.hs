{-# LANGUAGE OverloadedStrings #-}

module Hercules.Evaluate.Service
  ( evalOne
  , monitorDatabase
  , unlockJobsets
  ) where

import System.Process.Typed
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit

import Hercules.Evaluate.Types

hydraEvaluator :: [Text] -> IO ()
hydraEvaluator args = runProcess (proc "hydra-evaluator" args') >>= exitWith
  where args' = map T.unpack args

evalOne :: ProjectName -> JobsetName -> IO ()
evalOne p js = hydraEvaluator [p, js]

monitorDatabase :: IO ()
monitorDatabase = hydraEvaluator []

unlockJobsets :: IO ()
unlockJobsets = hydraEvaluator ["--unlock"]
