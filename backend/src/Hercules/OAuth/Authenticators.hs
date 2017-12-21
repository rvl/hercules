{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}

{-|
A module providing data types and functions for getting information about
different authentication providers.
-}
module Hercules.OAuth.Authenticators
  ( AuthenticatorName(..)
  , OAuth2Authenticator
  , configAuthenticatorList
  , authenticationURLWithState
  , appendQueryParam
  ) where

import Data.Aeson
import Data.ByteString      (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe           (catMaybes)
import Data.Text.Encoding   (encodeUtf8)
import Network.OAuth.OAuth2
import URI.ByteString
import Data.Monoid
import Control.Lens

import Hercules.Config
import Hercules.OAuth.Authenticators.GitHub
import Hercules.OAuth.Authenticators.Google
import Hercules.OAuth.Types
import Hercules.ServerEnv                   (App)

-- | Get a list of usable authentication services from the given config.
configAuthenticatorList :: Config -> [OAuth2Authenticator App]
configAuthenticatorList Config{..} = catMaybes
  [ googleAuthenticator makeCallback <$> configGoogleAuthInfo
  , githubAuthenticator makeCallback <$> configGitHubAuthInfo
  ]
  where
    makeCallback :: AuthenticatorName -> URI
    makeCallback (AuthenticatorName name) =
      let authority = Authority Nothing (Host (encodeUtf8 configHostname)) (Just (Port configPort))
          path = "/auth-callback/" <> encodeUtf8 name
      in URI (Scheme "http") (Just authority) path mempty Nothing

-- | Get the URL to redirect clients to with the given state to roundtrip
authenticationURLWithState :: OAuth2Authenticator m -> AuthState -> UserAuthURL
authenticationURLWithState authenticator state =
  let stateBS = packAuthState state
      queryParams = authenticatorAuthQueryParams authenticator
                    ++ [("state", stateBS)]
      config = authenticatorConfig authenticator
  in UserAuthURL . serializeURIRef' $ authorizationUrl config `appendQueryParam` queryParams

packAuthState :: AuthState -> ByteString
packAuthState = toStrict . encode

appendQueryParam :: URI -> [(ByteString, ByteString)] -> URI
appendQueryParam u ps = over (queryL . queryPairsL) (++ ps) u
