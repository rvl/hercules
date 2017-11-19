module Hercules.Evaluate.Jobset
  ( evaluateJobset
  ) where

import Data.Text (Text)
import Control.Exception (try, IOException)
import Data.Either (isRight)

import Hercules.Evaluate.Types
import Hercules.Input.Git
import Hercules.Evaluate.Spec

evaluateJobset :: ProjectName -> JobsetName -> IO Evaluation
evaluateJobset project jobset = undefined

checkJobset :: ProjectName -> JobsetName -> IO Bool
checkJobset p j = try doCheck >>= (\r -> updateJobset r >> return (isRight r))
  where
    doCheck = evaluateJobset p j
    updateJobset :: Either IOException Evaluation -> IO ()
    updateJobset (Right _) = return () -- update jobsets lastcheckedtime = now(), err = ""
    updateJobset (Left e) = return () -- update jobsets errormsg = show e, errortime = now()


data FetchInput = FetchInput

fetchInput :: BuildInput -> IO FetchInput
fetchInput (BuildInputGit uri depth branch) = do
  res <- fetchInputGit uri depth branch
  return FetchInput
fetchInput (BuildInputNix expr) = return FetchInput
fetchInput (BuildInputValue val) = return FetchInput
fetchInput (BuildInputPreviousBuild p j n v) = fetchInputBuild p j n v
fetchInput (BuildInputPreviousEvaluation p j n v) = fetchInputEvaluation p j n v



-- fixme: implement
fetchInputBuild :: Text -> Text -> Text -> Text -> IO FetchInput
fetchInputBuild project jobset name value = return FetchInput

-- fixme: implement
fetchInputEvaluation :: Text -> Text -> Text -> Text -> IO FetchInput
fetchInputEvaluation project jobset name value = return FetchInput
