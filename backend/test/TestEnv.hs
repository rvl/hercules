{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestEnv where

import           Control.Exception (throwIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import qualified Database.Postgres.Temp       as Temp
import Data.Monoid
import Data.Default
import Control.Monad.Except
import Say

import Hercules.Config
import Hercules.ServerEnv
import Hercules.OAuth.Authenticators (configAuthenticatorList)

-- | For github to sign web hook requests
testWebHookKey :: ByteString
testWebHookKey = "9dcc1cab304e6f340f56682fbeb6f9f54b80d0c0"

gitHubAuthClientInfo :: AuthClientInfo
gitHubAuthClientInfo = AuthClientInfo "2b0ca4f6f00dbf5e497c" "566a28e98ffa6e073c2e8f7ffea05d536e049513"

data TestEnv = TestEnv
  { tempDB     :: Temp.DB
  -- ^ Handle for temporary @postgres@ process
  , testEnv :: Env
  -- ^ Application environment
  }

-- | Start a temporary @postgres@ process and hook up the app
setupEnv :: IO TestEnv
setupEnv = do
  tempDB <- either throwIO return =<< Temp.startAndLogToTmp []
  let connStr = T.pack (Temp.connectionString tempDB)
  sayErr $ "Using temporary db: " <> connStr
  let cfg = def { configDatabaseConnectionString = connStr
                , configGitHubAuthInfo = Just gitHubAuthClientInfo
                , configSecretKeyFile = "test_secret.key"
                , configGitHubWebHookSecretFile = Just $ S8.unpack testWebHookKey }
      authenticators = configAuthenticatorList cfg
  Just testEnv <- newEnvTest cfg authenticators
  return TestEnv {..}


-- | Drop all the connections and shutdown the @postgres@ process
teardownEnv :: TestEnv -> IO ()
teardownEnv TestEnv {..} = do
  deleteEnv testEnv
  void $ Temp.stop tempDB
