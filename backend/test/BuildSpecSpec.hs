{-# LANGUAGE OverloadedStrings #-}

module BuildSpecSpec where

import Test.Hspec
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Monoid
import Data.Yaml (decodeEither', ParseException(..))
import qualified Data.Map as M
import Network.URI (parseURI)
import Data.Maybe (fromJust)
import Hercules.Evaluate.Spec

spec :: Spec
spec = do
  describe "Build spec parsing" $ do
    it "works for basic .hercules.yml" $
      decodeEither' basicYAML `shouldBe` Right basicBuildSpec

    it "works for really small .hercules.yml" $
      decodeEither' smallYAML `shouldBe` Right smallBuildSpec

instance Eq ParseException where
  a == b = show a == show b

basicYAML :: ByteString
basicYAML = "inputs:\n  nixpkgs:\n    type: git\n    value: git://github.com/NixOS/nixpkgs.git\n    shallow: false\n    branch: master\n\njobset: release.nix\n\nnotifications:\n  - email:\n    - hercules-ci@company.com\n"

basicBuildSpec :: BuildSpec
basicBuildSpec = BuildSpec
  { buildInputs = M.fromList [("nixpkgs",InputSpecGit {buildInputGitRemoteURI = fromJust (parseURI "git://github.com/NixOS/nixpkgs.git"), buildInputGitDepth = CloneDeep, buildInputGitBranch = Nothing})]
  , buildJobset = "release.nix"
  , buildNotifications = [BuildNotificationEmail ["hercules-ci@company.com"]]
  }

smallYAML :: ByteString
smallYAML = "jobset: release.nix"

smallBuildSpec :: BuildSpec
smallBuildSpec = BuildSpec
  { buildInputs = mempty
  , buildJobset = "release.nix"
  , buildNotifications = []
  }
