{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module GitHub.Data.Extra
 ( Installation
 , InstallationAccessToken(..)
 , installationAccessTokenAuth
 , BearerToken
 ) where

import Data.ByteString (ByteString)
import Data.Text.Encoding      (encodeUtf8)
import GitHub.Auth
import GitHub.Data.Definitions
import GitHub.Data.Id          (Id)
import GitHub.Data.Name        (Name)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

type BearerToken = ByteString

data Installation

data InstallationAccessToken = InstallationAccessToken
  { tokenText :: !Token
  , tokenExpiresAt :: !UTCTime
  } deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData InstallationAccessToken where rnf = genericRnf
instance Binary InstallationAccessToken

instance FromJSON InstallationAccessToken where
    parseJSON = withObject "InstallationAccessToken" $ \o -> InstallationAccessToken
      <$> (encodeUtf8 <$> o .: "token")
      <*> o .: "expires_at"

-- https://api.github.com/installations/:installation_id/access_tokens

installationAccessTokenAuth :: InstallationAccessToken -> Auth
installationAccessTokenAuth = OAuth . tokenText
