{-# LANGUAGE OverloadedStrings #-}

module Hercules.Evaluate.ReleaseExpr
  ( evaluateReleaseExpr
  , EvalResult, AttrPath
  , JobEvalResult(..)
  , ReleaseExprLang(..)
  , pathExprLang
  , hashReleaseExpr
  , evalErrorMsgs
  ) where

import System.Process.Typed
import System.Exit (ExitCode(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson
import Data.List (isSuffixOf, sortOn)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe (maybeToList)
import Control.Applicative
import Hercules.Evaluate.Spec
import Hercules.Evaluate.Fetch
import Hercules.Util

data ReleaseExprLang = ReleaseExprNix | ReleaseExprGuile deriving (Show, Eq)

type AttrPath = Text -- ^ strings separated by dots
type EvalResult = Map AttrPath (Either String JobEvalResult)

data JobEvalResult = JobEvalResult
                     { evalResultNixName      :: Text
                     , evalResultSystem       :: Text
                     , evalResultDrvPath      :: FilePath
                     , evalResultDescription  :: Text
                     , evalResultLicense      :: [Text]
                     , evalResultHomepage     :: [Text]
                     , evalResultMaintainers  :: [Text]
                     , evalResultSchedulingPriority :: Int
                     , evalResultTimeout      :: Int
                     , evalResultMaxSilent    :: Int
                     , evalResultIsChannel    :: Bool
                     , evalResultConstituents :: [Text]
                     , evalResultOutputs      :: Map Text Text
                     }
                   deriving (Show, Eq)

-- | Evaluates the release.nix jobset by running hydra-eval-jobs.
evaluateReleaseExpr :: ReleaseExprLang -- ^ Type of expression
                    -> Maybe FilePath  -- ^ GC roots directory
                    -> FilePath        -- ^ Absolute path to nix file
                    -> BuildInputs
                    -> IO (Either String EvalResult)
evaluateReleaseExpr lang gcRootsDir releaseExpr inputInfo = do
  let
    gcRootsArg = maybe [] (\d -> ["--gc-roots-dir", d]) gcRootsDir
    args = gcRootsArg ++ [releaseExpr] ++ inputArgs lang inputInfo
  (code, out, err) <- readProcess (proc (evalJobs lang) args)
  return $ case code of
    ExitSuccess -> case eitherDecode out of
      Right res -> Right (M.map unEvalOutputAttr res)
      Left msg -> Left ("Decoding JSON failed: " ++ msg)
    ExitFailure n -> Left (evalJobs lang <> " returned " <> show n <> ": " <> (T.unpack (decodeUtf8 (BL8.toStrict err)))) -- fixme: better error message

pathExprLang :: FilePath -> ReleaseExprLang
pathExprLang p | ".scm" `isSuffixOf` p = ReleaseExprGuile
               | otherwise = ReleaseExprNix

evalJobs :: ReleaseExprLang -> String
evalJobs ReleaseExprNix = "hydra-eval-jobs"
evalJobs ReleaseExprGuile = "hydra-eval-guile-jobs"

-- | Hash the arguments to hydra-eval-jobs in order to see if the
-- previous evaluation had the same inputs.
hashReleaseExpr :: ReleaseExprLang
                -> FilePath    -- ^ Absolute path to nix file
                -> BuildInputs -- ^ Fetched inputs
                -> Text -- ^ SHA-256 hash as hexadecimal string
hashReleaseExpr lang p inputs = T.pack $ sha256Hex (p:inputArgs lang inputs)

inputArgs :: ReleaseExprLang -> BuildInputs -> [String]
inputArgs lang = map T.unpack . concatMap args . sortOn fst . M.assocs
  where
    args (arg, input) = maybeToList (pathArg arg input) ++ args2 arg input
    pathArg name input = fmap (\p -> name <> "=" <> T.pack p) (buildInputStorePath input)

    args2 arg (BuildInputString (InputSpecString v)) = ["--argstr", arg, v]
    args2 arg (BuildInputBoolean (InputSpecBoolean v)) = ["--arg", arg, boolArg v]
    args2 arg (BuildInputNix (InputSpecNix v)) = ["--arg", arg, v]
    args2 arg (BuildInputPreviousEvaluation _ m) = ["--arg", arg, evaluationExpr m]
    args2 arg (BuildInputGit _ p _ m) =
      ["--arg", arg, buildInputToString lang "git" p Nothing m]
    args2 _ _ = []

    boolArg v = if v then "true" else "false"
    evaluationExpr m = mconcat $ ["{ "] ++ map attr (M.assocs m) ++ ["}"]
      where attr (k, v) = k <> " = builtins.storePath " <> T.pack v <> ";"

buildInputToString :: ReleaseExprLang -> Text -> NixStorePath -> Maybe Text -> Map Text Text -> Text
buildInputToString ReleaseExprNix inputType storePath drvPath inputs = mconcat . mconcat $ [
  [ "{ outPath = builtins.storePath ", T.pack storePath
  , attr "inputType" inputType ],
  [attr at (inputs ! i) | (at, i) <- attrs, M.member i inputs],
  [ maybe "" (\p -> attr "drvPath" ("builtins.storePath " <> p)) drvPath
  , ";}" ] ]
  where
    attr name val = "; " <> name <> " = \"" <> val <> "\""
    attrs = [ ("uri", "uri")
            , ("rev", "revNumber")
            , ("rev", "revision")
            , ("revCount", "revCount")
            , ("gitTag", "gitTag")
            , ("shortRev", "shortRev")
            , ("version", "version")
            , ("outputName", "outputName")
            ]

buildInputToString ReleaseExprGuile _ _ _ _ = "(fixme)"

-- | Evaluation program returns a mapping of attr paths to
-- entries which are either
--   - object with single error attribute
--   - object with job eval result
-- Job eval entries are wrapped in a newtype to help json parsing.
newtype EvalOutputAttr = EvalOutputAttr { unEvalOutputAttr :: Either String JobEvalResult }

instance FromJSON EvalOutputAttr where
  parseJSON o = EvalOutputAttr <$> (Left <$> parseError o <|> Right <$> parseJSON o)
    where parseError = withObject "error message" (\e -> e .: "error")

instance FromJSON JobEvalResult where
  parseJSON = withObject "evaluation result" $ \o ->
    JobEvalResult <$>
    o .: "nixName" <*>
    o .: "system" <*>
    o .: "drvPath" <*>
    o .: "description" <*>
    (csv <$> o .: "license") <*>
    (csv <$> o .: "homepage") <*>
    (csv <$> o .: "maintainers") <*>
    o .: "schedulingPriority" <*>
    o .: "timeout" <*>
    o .: "maxSilent" <*>
    o .: "isChannel" <*>
    (csv <$> o .: "constituents") <*>
    o .: "outputs"

csv :: Text -> [Text]
csv = map T.strip . T.splitOn ","

evalErrorMsgs :: EvalResult -> Text
evalErrorMsgs eval = mconcat (map (uncurry jobErrorMsg) (M.assocs eval))
  where
    jobErrorMsg :: AttrPath -> Either String a -> Text
    jobErrorMsg _ (Right _) = ""
    jobErrorMsg jobName (Left e) = w <> ":\n" <> (T.pack e) <> "\n\n"
      where w | T.null jobName = "at top-level"
              | otherwise = "in job '" <> jobName <> "'"
