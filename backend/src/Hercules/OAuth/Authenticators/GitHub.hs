{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE QuasiQuotes       #-}

{-|
GitHub specific OAuth2 functionality
-}
module Hercules.OAuth.Authenticators.GitHub
  ( githubAuthenticator
  ) where

import Control.Monad.Log
import Data.Aeson.TH
import Data.Semigroup
import Data.Text            hiding (tail)
import Data.Text.Encoding   (encodeUtf8)
import Network.HTTP.Client  (Manager)
import Network.OAuth.OAuth2
import URI.ByteString
import URI.ByteString.QQ

import Hercules.Config            (AuthClientInfo (..))
import Hercules.Database.Hercules
import Hercules.Encryption
import Hercules.Log
import Hercules.OAuth.Types
import Hercules.OAuth.User
import Hercules.Query.Hercules
import Hercules.ServerEnv

{-# ANN module ("HLint: Ignore Use CamelCase" :: String) #-}


data GitHubUser =
  GitHubUser { gid    :: Integer
             , gname  :: Text
             , gemail :: Text
             }
  deriving (Show, Eq)

deriveJSON defaultOptions{fieldLabelModifier = tail} ''GitHubUser

githubAuthenticator
  :: (AuthenticatorName -> URI)
  -> AuthClientInfo
  -> OAuth2Authenticator App
githubAuthenticator makeCallback clientInfo =
  makeAuthenticator makeCallback
                    (AuthenticatorName "github")
                    githubScopeEmail
                    githubOAuthEndpoint
                    githubAccessTokenEndpoint
                    clientInfo
                    githubGetUserInfo

githubScopeEmail :: QueryParams
githubScopeEmail = [("scope", "user:email")]

githubOAuthEndpoint :: OAuthEndpoint
githubOAuthEndpoint = OAuthEndpoint [uri|https://github.com/login/oauth/authorize|]

githubAccessTokenEndpoint :: AccessTokenEndpoint
githubAccessTokenEndpoint = AccessTokenEndpoint [uri|https://github.com/login/oauth/access_token|]

githubGetUserInfo :: AccessToken -> App (Either Text UserId)
githubGetUserInfo token = do
  withHttpManager (\m -> getUserInfo m token) >>= \case
    Left _err  -> pure $ Left "Error getting user info"
    Right user -> findOrCreateUser user token

findOrCreateUser :: GitHubUser -> AccessToken -> App (Either Text UserId)
findOrCreateUser user token = do
  let textId = pack . show . gid $ user
  runHerculesQueryWithConnection (userGitHubIdQuery textId) >>= \case
    []  -> createUser user token
    [u] -> pure $ Right (UserId (userId (u :: User)))
    _   -> pure $ Left "Multiple users with the same id in database!"

createUser :: GitHubUser -> AccessToken -> App (Either Text UserId)
createUser GitHubUser{..} token = do
  encryptedToken <- encrypt (encodeUtf8 . atoken $ token)
  let user = User () gname gemail (pack . show $ gid) encryptedToken
  withHerculesConnection (\c -> insertUser c user) >>= \case
    Nothing -> pure $ Left "Error inserting user"
    Just i -> do
      logInfo (LogString ("Added user " <> gname <> " to database"))
      pure $ Right i

getUserInfo :: Manager -> AccessToken -> IO (OAuth2Result String GitHubUser)
getUserInfo manager token = do
  authGetJSON manager token [uri|https://api.github.com/user|]
