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
import Servant.Utils.Enter                  (enter)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Internal (withApplication)
import           Test.Hspec.Wai.JSON
import qualified Database.Postgres.Temp       as Temp
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteArray.Encoding as B (Base (..), convertToBase)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Crypto.MAC.HMAC
import Crypto.Hash (SHA1)
import Data.Monoid
import Data.Default
import Control.Monad.Except
import Say
import Data.Aeson (FromJSON(..), decode)

import           GitHub.Data.PullRequests
import           GitHub.Data.Webhooks

import Hercules.Config
import Hercules.ServerEnv
import Hercules.Lib (appContext, appToHandler)
import Hercules.Hooks.GitHub
import Hercules.Database.Hercules (User'(..), GithubRepo'(..), GithubRepo)
import Hercules.Query.Hercules (getGitHubAppId, insertUser)
import Hercules.OAuth.Types (PackedJWT(..))
import Hercules.Lib hiding (app)
import Hercules.API (API(..))
import TestEnv

spec :: Spec
spec = do
  beforeAll setupEnv $ afterAll teardownEnv $ do
    describe "GitHub WebHook" $ do
      it "GET responds with 405" $ withApp $ do
        get "/github/webhook" `shouldRespondWith` 405

      it "empty POST responds with 400" $ withApp $ do
        let payload = ""
        request "POST" "/github/webhook" (signedPostHeaders payload) payload `shouldRespondWith` 400

      -- fixme: implement tests where webhook post results in queued build

    describe "GitHub WebHook Ping" $ do
      it "authenticates signed ping" $ withApp $ do
        ping <- liftIO getPingRequest
        let hdrs = (signedPingHeaders ping)
        request "POST" "/github/webhook" hdrs ping `shouldRespondWith` 200

      it "denies ping with wrong sig" $ withApp $ do
        ping <- liftIO getPingRequest
        request "POST" "/github/webhook" (signedPingHeaders "asdf") ping `shouldRespondWith` 401

      it "denies ping with missing sig" $ withApp $ do
        ping <- liftIO getPingRequest
        request "POST" "/github/webhook" pingHeaders ping `shouldRespondWith` 401

      it "stores app_id in database" $ \TestEnv{..} -> withApplication (app testEnv) $ do
        ping <- liftIO getPingRequest
        _ <- request "POST" "/github/webhook" (signedPingHeaders ping) ping
        -- https://begriffs.com/posts/2014-10-19-warp-server-controller-test.html
        liftIO $ do
          -- fixme: should query rest api rather than the database
          appId <- runExceptT (runApp testEnv $ withHerculesConnection getGitHubAppId)
          appId `shouldBe` (Right (Just 7071))

      it "updates app_id in database" $ \TestEnv{..} -> withApplication (app testEnv) $ do
        ping <- liftIO getPingRequest
        _ <- request "POST" "/github/webhook" (signedPingHeaders ping) ping
        request "POST" "/github/webhook" (signedPingHeaders ping) ping `shouldRespondWith` 200
        get "/github/registration" `shouldRespondWith` 200 { matchBody = MatchBody (containsStr "7071") }
        -- fixme: use different app_id and check that it's updated

    describe "GitHub webhook Integration" $ do
      it "stores installation id in database" $ \TestEnv{..} -> withApplication (app testEnv) $ do
        -- setup app id
        ping <- liftIO getPingRequest
        _ <- request "POST" "/github/webhook" (signedPingHeaders ping) ping
        -- send installation webhook event
        inst <- liftIO getInstallationRequest
        request "POST" "/github/webhook" (signedInstallationHeaders inst) inst `shouldRespondWith` 200
        -- check the protected repos api for results of installation
        Right tok <- liftIO $ runExceptT (runApp testEnv makeTestUserJWT)
        request "GET" "/protected/repos" (protectedHeaders tok) "" `shouldRespondWith`
          matchJSON (matchRepos 73241)

matchJSON :: FromJSON a => (a -> Maybe String) -> ResponseMatcher
matchJSON exp = ResponseMatcher 200 [MatchHeader isJson] (MatchBody matchBody)
  where
    isJson hdrs _ | hasJsonContentType hdrs = Nothing
                  | otherwise = Just "content-type wasn't application/json"
    hasJsonContentType hdrs = fromMaybe False (S8.isPrefixOf "application/json" <$> lookup "Content-Type" hdrs)
    matchBody _ body = case decode body of
      Just a -> exp a
      Nothing -> Just "Couldn't decode body as JSON"

matchRepos :: Int -> [GithubRepo] -> Maybe String
matchRepos _ [] = Just "Expected some repos"
matchRepos inst (r:rs) | githubRepoInstallationId r == Just inst = Nothing
                       | otherwise = Just "wrong installation id"

makeTestUserJWT :: App PackedJWT
makeTestUserJWT = do
  let u = User Nothing "Test User" "test@test.com" "" ""
  Just uid <- withHerculesConnection $ \c -> insertUser c u
  Right jwt <- makeUserJWT uid
  return jwt

protectedHeaders :: PackedJWT -> [Header]
protectedHeaders jwt = [ ("Accept", "application/json")
                       , ("Authorization", "Bearer " <> unPackedJWT jwt)
                       ]

containsStr :: S8.ByteString -> [Header] -> Body -> Maybe String
containsStr s _ body | s `S8.isInfixOf` (BL8.toStrict body) = Nothing
                     | otherwise = Just $ "body mismatch: expected to find " ++ S8.unpack s

-- | For signing requests
testKey :: ByteString
testKey = "9dcc1cab304e6f340f56682fbeb6f9f54b80d0c0"

getPingRequest :: IO BL.ByteString
getPingRequest = BL.readFile "test/data/webhook-ping.json"

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

getInstallationRequest :: IO BL.ByteString
getInstallationRequest = BL.readFile "test/data/integration.json"

signedInstallationHeaders :: BL.ByteString -> [Header]
signedInstallationHeaders payload = (signatureHeader payload:hdrs)
  where hdrs =  [ ("X-GitHub-Event", "integration_installation")
                , ("content-type", "application/json")
                , ("Expect", "")
                , ("User-Agent", "GitHub-Hookshot/a322ef4")
                , ("X-GitHub-Delivery", "08f13700-de52-11e7-8c03-cdbe99d62a28")
                ]

withApplication' :: ActionWith Application -> ActionWith Env
withApplication' action env = action (app env)

-- https://github.com/hspec/hspec-wai/issues/36
withApp :: WaiSession a -> TestEnv -> IO a
withApp action TestEnv{..} = withApplication (app testEnv) action

app :: Env -> Application
app env = serveWithContext api (appContext env) (server env)
  where api = Proxy :: Proxy API

appOnlyGitHub :: Env -> Application
appOnlyGitHub env = serveWithContext api (appContext env) (gitHubAppServer env)
  where api = Proxy :: Proxy GitHubAppAPI

-- | Test the GitHub hook API by itself.
gitHubAppServer :: Env -> Server GitHubAppAPI
gitHubAppServer env = enter (appToHandler env) gitHubAppApi

-- | Creates a signature header as described in
-- https://developer.github.com/webhooks/securing/
gitHubSignature :: ByteString -> ByteString
gitHubSignature body = "sha1=" <> B.convertToBase B.Base16 mac
  where mac = hmac testKey body :: HMAC SHA1

signatureHeader :: BL.ByteString -> Header
signatureHeader payload = ("X-Hub-Signature", gitHubSignature (BL.toStrict payload))
