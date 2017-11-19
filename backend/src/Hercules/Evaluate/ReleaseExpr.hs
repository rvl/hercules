{-# LANGUAGE OverloadedStrings #-}

module Hercules.Evaluate.ReleaseExpr
  ( evaluateReleaseExpr
  , EvalResult(..)
  ) where

import System.Process.Typed
import System.Exit (ExitCode(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson

-- | Evaluates the release.nix jobset by running hydra-eval-jobs.
evaluateReleaseExpr :: Maybe FilePath -- ^ GC roots directory
                    -> FilePath       -- ^ Absolute path to nix file
                    -> IO EvalResult
evaluateReleaseExpr gcRootsDir releaseExpr = do
  let args = (maybe [] (\d -> ["--gc-roots-dir", d]) gcRootsDir) ++ [releaseExpr]
  (code, out, err) <- readProcess (proc "hydra-eval-jobs" args)
  return $ case code of
    ExitSuccess -> case eitherDecode out of
      Right res -> res
      Left msg -> EvalError (T.pack ("Decoding JSON failed: " ++ msg))
    ExitFailure _ -> EvalError (decodeUtf8 (BL8.toStrict err))

data EvalResult = EvalSuccess
                  { evalResultNixName     :: Text
                  , evalResultSystem      :: Text
                  , evalResultDrvPath     :: FilePath
                  , evalResultDescription :: Text
                  , evalResultLicense     :: [Text]
                  , evalResultHomepage    :: [Text]
                  , evalResultMaintainers :: [Text]
                  , evalResultSchedulingPriority :: Int
                  , evalResultTimeout     :: Int
                  , evalResultMaxSilent   :: Int
                  , evalResultIsChannel   :: Bool
                  }
                | EvalFailure { evalResultFailureMsg :: Text }
                | EvalError { evalResultErrorMsg :: Text }
                deriving (Show, Eq)

instance FromJSON EvalResult where
  parseJSON = withObject "evaluation result" $ \o -> do
    err <- o .:? "error"
    case err of
      Nothing -> EvalSuccess <$> o .: "nixName" <*>
                 o .: "system" <*>
                 o .: "drvPath" <*>
                 o .: "description" <*>
                 (csv <$> o .: "license") <*>
                 (csv <$> o .: "homepage") <*>
                 (csv <$> o .: "maintainers") <*>
                 o .: "schedulingPriority" <*>
                 o .: "timeout" <*>
                 o .: "maxSilent" <*>
                 o .: "isChannel"
      Just msg -> pure (EvalFailure msg)

csv :: Text -> [Text]
csv = map T.strip . T.splitOn ","
