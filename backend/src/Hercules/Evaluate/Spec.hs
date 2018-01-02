{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Hercules.Evaluate.Spec
  ( BuildSpec(..)
  , InputSpec(..)
  , InputSpecs
  , inputSpecType
  , inputSpecValue
  , inputSpecUri
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

type InputSpecs = Map Text InputSpec
-- | A declarative build specification.
-- This is declared in the file .hercules.yml within the source repository.
data BuildSpec = BuildSpec { buildInputs :: InputSpecs
                             -- ^ Additional sources by name
                           , buildJobset :: FilePath
                             -- ^ Path to a nix file within source repository
                           , buildNotifications :: [BuildNotification]
                             -- ^ Optional additional notification methods
                           } deriving (Show, Eq, Generic)

-- | A build input is provided by the build system as a parameter to the
-- jobset nix file.
-- TODO: add mercurial input and others
-- TODO: make it more pluggable
data InputSpec = InputSpecGit
                  { buildInputGitRemoteURI :: URI
                  , buildInputGitDepth :: CloneDepth
                  , buildInputGitBranch :: Maybe Text
                  }
                  -- ^ Fetches a git repository
                | InputSpecNix Text
                  -- ^ A nix expression string
                | InputSpecBoolean Bool -- ^ A flag parameter
                | InputSpecString Text -- ^ A string parameter
                | InputSpecValue Value
                  -- ^ Any value supported by YAML, e.g. a boolean or a string
                | InputSpecPreviousBuild Text
                | InputSpecPreviousEvaluation Text
                deriving (Show, Eq, Generic)

-- | How to send notifications to users when a build finishes.
-- TODO: add a pluggable BuildNotificationOther Text JSON.Value.
data BuildNotification = BuildNotificationEmail [Text] deriving (Show, Eq, Generic)

instance FromJSON InputSpec where
  parseJSON = withObject "InputSpec" $ \ob -> do
    t <- ob .: "type"
    val <- ob .: "value"
    make t val
    where
      make :: Text -> Value -> Parser InputSpec
      make "git" = withText "git remote url"
                   (\v -> case parseURI (T.unpack v) of
                            Nothing -> fail "Not a URI"
                            Just u -> pure (InputSpecGit u CloneDeep Nothing))
      make "nix" = withText "nix expression string" (pure . InputSpecNix) -- fixme: could maybe parse nix?
      make "boolean" = withBool "boolean" (pure . InputSpecBoolean)
      make "string" = withText "string" (pure . InputSpecString)
      make "value" = pure . InputSpecValue
      make "build" = pure . const (InputSpecPreviousBuild "")
      make "eval" = pure . const (InputSpecPreviousEvaluation "")
      make _ = fail "Unknown type"

-- fixme: update for new fields
instance ToJSON InputSpec where
  toJSON (InputSpecGit uri depth branch) = buildInputToValue "git" (T.pack $ show uri)
  toJSON (InputSpecNix expr) = buildInputToValue "nix" expr
  toJSON (InputSpecBoolean b) = toJSON (InputSpecValue (Bool b))
  toJSON (InputSpecString s) = toJSON (InputSpecValue (String s))
  toJSON (InputSpecValue v) = buildInputToValue "value" v
  toJSON (InputSpecPreviousBuild _) = object [ "type" .= ("build" :: Text) ]
  toJSON (InputSpecPreviousEvaluation _) = object [ "type" .= ("eval" :: Text) ]

buildInputToValue :: ToJSON a => Text -> a -> Value
buildInputToValue t v = object [ "type" .= t, "value" .= toJSON v ]

instance FromJSON BuildSpec where
  parseJSON = withObject "BuildSpec" $ \o -> BuildSpec <$>
    o .:? "inputs" .!= mempty <*>
    o .: "jobset" <*>
    o .:? "notifications" .!= mempty

instance ToJSON BuildSpec where toJSON = genericToJSON prefixOptions

instance FromJSON BuildNotification where parseJSON = genericParseJSON prefixOptions
instance ToJSON BuildNotification where toJSON = genericToJSON prefixOptions

inputSpecType :: InputSpec -> Text
inputSpecType (InputSpecGit _ _ _)            = "git"
inputSpecType (InputSpecNix _)                = "nix"
inputSpecType (InputSpecBoolean _)            = "boolean"
inputSpecType (InputSpecString _)             = "string"
inputSpecType (InputSpecValue _)              = "value"
inputSpecType (InputSpecPreviousBuild _)      = "build"
inputSpecType (InputSpecPreviousEvaluation _) = "eval"

inputSpecValue :: InputSpec -> Text
inputSpecValue (InputSpecGit _ _ _)            = "fixme"
inputSpecValue (InputSpecNix _)                = "fixme"
inputSpecValue (InputSpecBoolean _)            = "fixme"
inputSpecValue (InputSpecString _)             = "fixme"
inputSpecValue (InputSpecValue _)              = "fixme"
inputSpecValue (InputSpecPreviousBuild _)      = "fixme"
inputSpecValue (InputSpecPreviousEvaluation _) = "fixme"

inputSpecUri :: InputSpec -> Maybe URI
inputSpecUri (InputSpecGit u _ _) = Just u
inputSpecUri _ = Nothing
