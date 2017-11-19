module Main where

import System.Environment
import System.Exit
import Hercules.Evaluate.Jobset
import Data.Text (pack)

main :: IO ()
main = getArgs >>= \args -> case args of
  [projectName, jobsetName] -> do
    _ <- evaluateJobset (pack projectName) (pack jobsetName)
    exitSuccess
  _ -> do
    prog <- getProgName
    die $ "syntax: " ++ prog ++ " <PROJECT> <JOBSET>"
