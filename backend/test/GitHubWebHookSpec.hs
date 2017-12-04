{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GitHubWebHookSpec where

import           Control.Exception (throwIO)
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.Types
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp hiding (withApplication)
import           Servant hiding (Header)
-- import Control.Natural
import Servant.Utils.Enter                  (enter)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Internal (withApplication)
import           Test.Hspec.Wai.JSON
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteArray.Encoding as B (Base (..), convertToBase)
import Crypto.MAC.HMAC
import Crypto.Hash (SHA1)
import Data.Monoid
import Data.Default
import Control.Monad.Except

import           GitHub.Data.PullRequests
import           GitHub.Data.Webhooks

import Hercules.Config
import Hercules.ServerEnv
import Hercules.Lib (appContext, appToHandler)
import Hercules.Hooks.GitHub
import Hercules.Query.Hercules (getGitHubAppId)

spec :: Spec
spec = do
  beforeAll makeTestEnv $ do
    describe "GitHub WebHook" $ do
      it "GET responds with 405" $ \env -> withApplication (app env) $ do
        get "/github/webhook" `shouldRespondWith` 405

      it "empty POST responds with 400" $ withApp $ do
        let payload = ""
        request "POST" "/github/webhook" (signedPostHeaders payload) payload `shouldRespondWith` 400

      -- fixme: implement some tests

    describe "GitHub WebHook Ping" $ do
      it "authenticates signed ping" $ withApp $ do
        ping <- liftIO getPingRequest
        request "POST" "/github/webhook" (signedPingHeaders ping) ping `shouldRespondWith` 200

      it "denies ping with wrong sig" $ withApp $ do
        ping <- liftIO getPingRequest
        request "POST" "/github/webhook" (signedPingHeaders "asdf") ping `shouldRespondWith` 401

      it "denies ping with missing sig" $ withApp $ do
        ping <- liftIO getPingRequest
        request "POST" "/github/webhook" pingHeaders ping `shouldRespondWith` 401

      it "stores app_id in database" $ \env -> withApplication (app env) $ do
        ping <- liftIO getPingRequest
        _ <- request "POST" "/github/webhook" (signedPingHeaders ping) ping
        -- https://begriffs.com/posts/2014-10-19-warp-server-controller-test.html
        liftIO $ do
          appId <- runExceptT (runApp env $ withHerculesConnection getGitHubAppId)
          appId `shouldBe` (Right (Just 7071))

-- | For signing requests
testKey :: ByteString
testKey = "9dcc1cab304e6f340f56682fbeb6f9f54b80d0c0"

getPingRequest :: IO BL.ByteString
getPingRequest = BL.readFile "test/webhook-ping.json"

signedPingHeaders :: BL.ByteString -> [Header]
signedPingHeaders ping = (signatureHeader ping:pingHeaders)

pingHeaders :: [Header]
pingHeaders = [ ("X-GitHub-Event", "ping")
              , ("User-Agent", "GitHub-Hookshot/e20df6f")
              , ("X-GitHub-Delivery", "cca9fe30-d47e-11e7-8a72-b4ac20fcb0b9")
              , ("content-type", "application/json")
              , ("Expect", "")
              ]

signedPostHeaders :: BL.ByteString -> [Header]
signedPostHeaders payload = (signatureHeader payload:postHeaders)

postHeaders :: [Header]
postHeaders = [ ("X-GitHub-Event", "pull_request")
              , ("content-type", "application/json")
              ]

withApplication' :: ActionWith Application -> ActionWith Env
withApplication' action env = action (app env)

-- https://github.com/hspec/hspec-wai/issues/36
withApp :: WaiSession a -> Env -> IO a
withApp action env = withApplication (app env) action

app :: Env -> Application
app env = serveWithContext api (appContext env) (gitHubAppServer env)
  where api = Proxy :: Proxy GitHubAppAPI

-- | Set up the application environment with a test database.
makeTestEnv :: IO Env
makeTestEnv = do
  -- fixme: use tmp-postgres library to setup database
  let cfg = def { configDatabaseConnectionString = "postgresql:///hercules_test"
                , configSecretKeyFile = "test_secret.key" }
  Just env <- newEnv cfg []
  return $ env { envGitHubWebHookSecret = Just testKey }

-- | Test the GitHub hook API by itself.
gitHubAppServer :: Env -> Server GitHubAppAPI
gitHubAppServer env = enter (appToHandler env) api
  where api = gitHubWebHookPR :<|> gitHubWebHookPing

-- | Creates a signature header as described in
-- https://developer.github.com/webhooks/securing/
gitHubSignature :: ByteString -> ByteString
gitHubSignature body = "sha1=" <> B.convertToBase B.Base16 mac
  where mac = hmac testKey body :: HMAC SHA1

signatureHeader :: BL.ByteString -> Header
signatureHeader payload = ("X-Hub-Signature", gitHubSignature (BL.toStrict payload))
