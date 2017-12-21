{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GitHubOAuthSpec where

import           Network.HTTP.Types
import           Servant hiding (Header)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Internal (withApplication)
import           Network.Wai.Test
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteArray.Encoding as B (Base (..), convertToBase)

import Hercules.Lib (app)

import TestEnv

spec :: Spec
spec = do
  beforeAll setupEnv $ afterAll teardownEnv $ do
    describe "GitHub Login View" $ do
      it "GET responds with redirect" $ withApp $ do
        get "/login/github?state=state&frontendURL=frontendURL" `shouldRespondWith` 303 { matchHeaders = [matchRedirect] }

      {- don't want to hit github in tests
      it "GET callback does stuff" $ withApp $ do
        get "/auth-callback/github?code=7bb640f6c35e5dce5302&state=%7B%22authStateFrontendURL%22%3A%22http%3A%2F%2Flocalhost%3A8080%2Flogged-in%22%2C%22authStateClientState%22%3A%22my+state%22%7D" `shouldRespondWith` 303 -- bad verification code
      -}

matchRedirect :: MatchHeader
matchRedirect = MatchHeader (\hdrs _ -> check hdrs)
  where
    check hdrs = case lookup "Location" hdrs of
      Just l | "https://github.com/login/oauth/authorize" `S8.isPrefixOf` l -> Nothing
             | otherwise -> Just "no redirect"
      Nothing -> Just "no location header"

withApp :: WaiSession a -> TestEnv -> IO a
withApp action TestEnv{..} = app testEnv >>= flip withApplication action
