{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

module GitHub.Endpoints.Extra
  ( createInstallationAccessToken'
  , createInstallationAccessTokenR
  , module GitHub.Data.Extra
  ) where

import GitHub.Data
import GitHub.Data.Extra
import GitHub.Internal.Prelude
import GitHub.Request
import GitHub.Data.Request
import Prelude ()
import qualified Data.Text as T

import qualified Network.HTTP.Client          as HTTP
import Network.HTTP.Client
       (HttpException (..), Manager, Response (..),
       newManager, requestHeaders, managerModifyRequest)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Network.HTTP.Client (parseRequest, method, requestBody, requestHeaders)
import Network.HTTP.Types (statusIsSuccessful)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as S8
import Data.Aeson (eitherDecode')

createInstallationAccessToken'
    :: BearerToken -> Id Installation
    -> IO (Either Error InstallationAccessToken)
createInstallationAccessToken' jwt installation = do
  putStrLn $ "token " <> S8.unpack jwt
  manager <- newManager tlsManagerSettings
  request <- createInstallationAccessTokenRequest jwt installation
  response <- HTTP.httpLbs request manager
  putStrLn $ "response: " <> BL8.unpack (responseBody response)
  return $ tokenFromResponse response

createInstallationAccessTokenRequest jwt installation = putStrLn ("url " <> url) >> auth <$> parseRequest url
  where
    url = "https://api.github.com/installations/" <> show (untagId installation) <> "/access_tokens"
    auth req = req { method = "POST"
                   , requestBody = ""
                   , requestHeaders = [ ("Authorization", "Bearer " <> jwt)
                                      , ("User-agent", "hercules-ci/hercules")
                                      , ("Accept", "application/vnd.github.machine-man-preview+json") ]
                   }

tokenFromResponse response = case (statusIsSuccessful $ responseStatus response, eitherDecode' (responseBody response)) of
                               (True, Right token) -> Right token
                               (_, Left msg) -> Left (ParseError (T.pack msg))
                               (_, _) -> Left (UserError "failed http request")

createInstallationAccessToken'Broken
    :: BearerToken -> Id Installation
    -> IO (Either Error InstallationAccessToken)
createInstallationAccessToken'Broken jwt installation =
    executeRequestBearer jwt $ createInstallationAccessTokenR installation

executeRequestBearer :: BearerToken -> Request k a -> IO (Either Error a)
executeRequestBearer jwt r = do
  manager <- newManager (tlsManagerSettings { managerModifyRequest = addAuth jwt })
  executeRequestWithMgr manager (OAuth jwt) r -- fixme: Add Bearer to github Auth types

addAuth :: BearerToken -> HTTP.Request -> IO HTTP.Request
addAuth jwt req = pure $ req { requestHeaders = (auth:requestHeaders req) }
    where auth = ("Authorization", "Bearer " <> jwt)

-- | Exchange a signed JWT for a short-lived access token.
-- See <https://developer.github.com/apps/building-github-apps/authentication-options-for-github-apps/#authenticating-as-an-installation>
createInstallationAccessTokenR :: Id Installation -> Request 'RW InstallationAccessToken
createInstallationAccessTokenR installation =
  command Post ["installations", toPathPart installation, "access_tokens"] ""
