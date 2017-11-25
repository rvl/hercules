{-# LANGUAGE LambdaCase        #-}

module Main where

import System.Environment
import System.Exit
import Hercules.Evaluate.Jobset
import Data.Text                (pack)
import Data.Yaml                (decodeFileEither, prettyPrintParseException)
import System.IO                (hPutStrLn, stderr)
import Data.Maybe
import Control.Monad.Except

import Hercules.Config
import Hercules.ServerEnv

-- | Parse the config file given in HERCULES_CONFIG environment variable.
-- If files is incorrect then exit with 'exitFailure'.
-- This is not the ideal of way of starting evaluation.
-- hydra-evaluator.cc should be updated or assimilated into hercules.
getConfig :: IO Config
getConfig = do
  f <- fromMaybe "config.yaml" <$> lookupEnv "HERCULES_CONFIG"
  decodeFileEither f >>= \case
    Left err -> do
      hPutStrLn stderr (prettyPrintParseException err)
      exitFailure
    Right c -> pure c


main :: IO ()
main = getArgs >>= \case
  [projectName, jobsetName] -> do
    config <- getConfig
    newEnv config [] >>= \case
      Nothing -> do
        hPutStrLn stderr "Error starting application."
        exitFailure
      Just env -> checkJobsetIO projectName jobsetName env
  _ -> do
    prog <- getProgName
    die $ "syntax: " ++ prog ++ " <PROJECT> <JOBSET>"

checkJobsetIO :: String -> String -> Env -> IO ()
checkJobsetIO projectName jobsetName env = do
  res <- runExceptT $ runApp env (checkJobset (pack projectName) (pack jobsetName))
  case res of
    Left _ -> exitFailure
    Right False -> exitFailure
    Right True -> exitSuccess
