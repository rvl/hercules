{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TemplateHaskell    #-}

module Hercules.Config
  ( Config(..)
  , ConnectInfo(..)
  , AuthClientInfo(..)
  , HostName
  , AccessLogLevel(..)
  ) where

import Data.Aeson
import Data.Aeson.Extra           (prefixOptions)
import Data.Text                  (Text)
import Database.PostgreSQL.Simple (ConnectInfo (..))
import GHC.Generics
import Network.Wai.Handler.Warp   (Port)
import Data.Default

import Hercules.OAuth.Types

type HostName = Text

-- | Access logging level
data AccessLogLevel = Disabled | Enabled | Development
  deriving(Read, Show, Generic)

instance FromJSON AccessLogLevel

data Config = Config { configPort                     :: Port
                     , configHostname                 :: HostName
                     , configAccessLogLevel           :: AccessLogLevel
                     , configSecretKeyFile            :: FilePath
                     , configDatabaseConnectionString :: Text
                     , configGoogleAuthInfo           :: Maybe AuthClientInfo
                     , configGitHubAuthInfo           :: Maybe AuthClientInfo
                     , configGitHubWebHookSecretFile  :: Maybe FilePath
                     , configGitHubAppPrivateKeyFile  :: Maybe FilePath
                     , configDataPath                 :: FilePath
                     }
  deriving(Read, Show, Generic)

instance FromJSON Config where parseJSON = genericParseJSON prefixOptions

-- | Default instance for testing convenience
instance Default Config where
  def = Config { configPort                     = 8080
               , configHostname                 = "localhost"
               , configAccessLogLevel           = Enabled
               , configSecretKeyFile            = ""
               , configDatabaseConnectionString = "postgresql://hydra@/hydra"
               , configGoogleAuthInfo           = Nothing
               , configGitHubAuthInfo           = Nothing
               , configGitHubWebHookSecretFile  = Nothing
               , configGitHubAppPrivateKeyFile  = Nothing
               , configDataPath                 = "/var/lib/hercules"
               }
