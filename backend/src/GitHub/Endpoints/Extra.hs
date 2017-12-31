{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module GitHub.Endpoints.Extra
  ( createInstallationAccessToken'
  , createInstallationAccessTokenR
  , module GitHub.Data.Extra
  ) where

import GitHub.Data
import GitHub.Data.Extra
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

import qualified Network.HTTP.Client          as HTTP
import Network.HTTP.Client
       (HttpException (..), Manager, Response (..),
       newManager, requestHeaders, managerModifyRequest)
import Network.HTTP.Client.TLS  (tlsManagerSettings)

createInstallationAccessToken'
    :: BearerToken -> Id Installation
    -> IO (Either Error InstallationAccessToken)
createInstallationAccessToken' jwt installation =
    executeRequestBearer jwt $ createInstallationAccessTokenR installation

executeRequestBearer :: BearerToken -> Request k a -> IO (Either Error a)
executeRequestBearer jwt r = do
  manager <- newManager (tlsManagerSettings { managerModifyRequest = addAuth jwt })
  executeRequestWithMgr' manager (unsafeDropAuthRequirements r)

addAuth :: BearerToken -> HTTP.Request -> IO HTTP.Request
addAuth jwt req = pure $ req { requestHeaders = (auth:requestHeaders req) }
    where auth = ("Authorization", "Bearer " <> jwt)

-- | Exchange a signed JWT for a short-lived access token.
-- See <https://developer.github.com/apps/building-github-apps/authentication-options-for-github-apps/#authenticating-as-an-installation>
createInstallationAccessTokenR :: Id Installation -> Request 'RW InstallationAccessToken
createInstallationAccessTokenR installation =
  command Post ["installation", toPathPart installation, "access_tokens"] ""
