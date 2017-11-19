{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Hercules.Evaluate.Spec
  ( BuildSpec(..)
  , BuildInput(..)
  ) where

import Data.Aeson
import Data.Aeson.Types           (Parser)
import Data.Aeson.Extra           (prefixOptions)
import Data.Text                  (Text)
import qualified Data.Text as T
import GHC.Generics
import Data.Map                   (Map)
import Network.URI                (URI, parseURI)

import Hercules.Input.Git (CloneDepth(..))

-- | A declarative build specification.
-- This is declared in the file .hercules.yml within the source repository.
data BuildSpec = BuildSpec { buildInputs :: Map Text BuildInput
                             -- ^ Additional sources by name
                           , buildJobset :: Text
                             -- ^ Path to a nix file within source repository
                           , buildNotifications :: [BuildNotification]
                             -- ^ Optional additional notification methods
                           } deriving (Show, Eq, Generic)

-- | A build input is provided by the build system as a parameter to the
-- jobset nix file.
-- TODO: add mercurial input and others
-- TODO: make it more pluggable
data BuildInput = BuildInputGit
                  { buildInputGitRemoteURI :: URI
                  , buildInputGitDepth :: CloneDepth
                  , buildInputGitBranch :: Maybe Text
                  }
                  -- ^ Fetches a git repository
                | BuildInputNix { buildInputNixExpr :: Text }
                  -- ^ A nix expression string
                | BuildInputBoolean Bool -- ^ A flag parameter
                | BuildInputString Text -- ^ A string parameter
                | BuildInputValue { buildInputValue :: Value }
                  -- ^ Any value supported by YAML, e.g. a boolean or a string
                | BuildInputPreviousBuild
                  { buildInputPreviousBuildProject :: Text
                  , buildInputPreviousBuildJobset :: Text
                  , buildInputPreviousBuildName :: Text
                  , buildInputPreviousBuildValue :: Text
                  }
                | BuildInputPreviousEvaluation
                  { buildInputPreviousEvaluationProject :: Text
                  , buildInputPreviousEvaluationJobset :: Text
                  , buildInputPreviousEvaluationName :: Text
                  , buildInputPreviousEvaluationValue :: Text
                  }
                deriving (Show, Eq, Generic)

-- | How to send notifications to users when a build finishes.
-- TODO: add a pluggable BuildNotificationOther Text JSON.Value.
data BuildNotification = BuildNotificationEmail [Text] deriving (Show, Eq, Generic)

instance FromJSON BuildInput where
  parseJSON = withObject "BuildInput" $ \ob -> do
    t <- ob .: "type"
    val <- ob .: "value"
    make t val
    where
      make :: Text -> Value -> Parser BuildInput
      make "git" = withText "git remote url"
                   (\v -> case parseURI (T.unpack v) of
                            Nothing -> fail "Not a URI"
                            Just u -> pure (BuildInputGit u CloneDeep Nothing))
      make "nix" = withText "nix expression string" (pure . BuildInputNix) -- fixme: could maybe parse nix?
      make "bool" = withBool "bool" (pure . BuildInputBoolean)
      make "string" = withText "string" (pure . BuildInputString)
      make "value" = pure . BuildInputValue
      make "build" = pure . const (BuildInputPreviousBuild "" "" "" "")
      make "evaluation" = pure . const (BuildInputPreviousEvaluation "" "" "" "")
      make _ = fail "Unknown type"

-- fixme: update for new fields
instance ToJSON BuildInput where
  toJSON (BuildInputGit uri depth branch) = buildInputToValue "git" (T.pack $ show uri)
  toJSON (BuildInputNix expr) = buildInputToValue "nix" expr
  toJSON (BuildInputBoolean b) = toJSON (BuildInputValue (Bool b))
  toJSON (BuildInputString s) = toJSON (BuildInputValue (String s))
  toJSON (BuildInputValue v) = buildInputToValue "value" v
  toJSON (BuildInputPreviousBuild _ _ _ _) = object [ "type" .= ("previous build" :: Text) ]
  toJSON (BuildInputPreviousEvaluation _ _ _ _) = object [ "type" .= ("previous evaluation" :: Text) ]

buildInputToValue :: ToJSON a => Text -> a -> Value
buildInputToValue t v = object [ "type" .= t, "value" .= toJSON v ]

instance FromJSON BuildSpec where parseJSON = genericParseJSON prefixOptions
instance ToJSON BuildSpec where toJSON = genericToJSON prefixOptions

instance FromJSON BuildNotification where parseJSON = genericParseJSON prefixOptions
instance ToJSON BuildNotification where toJSON = genericToJSON prefixOptions
