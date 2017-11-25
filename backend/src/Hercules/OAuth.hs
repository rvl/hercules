{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hercules.OAuth
  ( AuthState(..)
  , AuthCode(..)
  , authCallback
  ) where

import           Control.Monad.Except.Extra
import           Data.Aeson
import           Data.ByteString            (ByteString)
import           Data.ByteString.Lazy       (fromStrict)
import           Data.Text
import           Data.Text.Encoding
import           Network.OAuth.OAuth2 hiding (error)
import           URI.ByteString
import           Servant hiding (URI)
import           Servant.Redirect

import Hercules.OAuth.Authenticators
import Hercules.OAuth.Types
import Hercules.ServerEnv

authCallback :: AuthenticatorName
             -> Maybe AuthCode
             -> Maybe AuthError
             -> AuthStatePacked
             -> App a
authCallback authName maybeCode maybeError packedState = do
  -- Extract the state
  state <- failWith err400 (unpackState packedState)

  case (maybeCode, maybeError) of
    (Nothing, Nothing)   -> throwError err400
    (Nothing, Just err)  -> handleError state err
    (Just code, Nothing) -> handleCode authName state code
    (Just _, Just _)     -> throwError err400

handleError :: AuthState
            -> AuthError
            -> App a
handleError state err = do
  redirectError (redirectURI state) (unAuthError err)

redirectURI :: AuthState -> URI
redirectURI = fromRight . parseURI strictURIParserOptions . encodeUtf8 . unFrontendURL . authStateFrontendURL
  where
    fromRight (Right r) = r
    fromRight (Left _) = error "fromRight"

handleCode :: AuthenticatorName
           -> AuthState
           -> AuthCode
           -> App a
handleCode authName state (AuthCode code) = do
  -- Can we handle this authenticator
  authenticator <- failWithM err404 (getAuthenticator authName)
  let config = authenticatorConfig authenticator

  let clientState = authStateClientState state
      target = redirectURI state
      failWithOA (OAuth2Error err _ _) = redirectError target msg
        where msg = either id (const "unknown error") err

  -- Get the access token for this user
  token <- either failWithOA pure
    =<< withHttpManager (\m -> fetchAccessToken m config (ExchangeToken code))

  -- Get the user info with the token
  user <- either (redirectError target) pure
    =<< authenticatorGetUserInfo authenticator (accessToken token)

  -- Create a JWT
  jwt <- either (const (redirectError target "Failed to create JWT")) pure
    =<< makeUserJWT user

  -- Return to the frontend
  redirectSuccess target jwt clientState

redirectError :: URI
              -> Text
              -- ^ An error message
              -> App a
redirectError uri message =
  let param = [("authFailure", encodeUtf8 message)]
  in redirectParams uri param

redirectSuccess :: URI
                -> PackedJWT
                -- ^ This user's token
                -> Maybe AuthClientState
                -> App a
redirectSuccess uri jwt state =
  let params = ("jwt", unPackedJWT jwt) :
               case state of
                 Nothing -> []
                 Just s  -> [("state", encodeUtf8 . unAuthClientState $ s)]
  in redirectParams uri params

redirectParams :: URI -> [(ByteString, ByteString)] -> App a
redirectParams uri params = redirectBS (serializeURIRef' $ uri `appendQueryParam` params)

unpackState :: AuthStatePacked -> Maybe AuthState
unpackState = decode . fromStrict . encodeUtf8 . unAuthStatePacked
