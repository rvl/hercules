{-# LANGUAGE OverloadedStrings #-}

module AppMBCSpec where

import Test.Hspec
import Control.Error
import Control.Monad.Trans.Control
import System.IO.Error
import Servant.Server
import Control.Monad.Except (throwError)

import Hercules.ServerEnv
import TestEnv

testControl :: App ()
testControl = control $ \runInBase -> runInBase (liftIO $ putStrLn "hello")

testControlException :: App ()
testControlException = control $ \runInBase -> runInBase (fail "io problem")

testControlServantErr :: App ()
testControlServantErr = control $ \runInBase -> runInBase (throwError err401)

spec :: Spec
spec = beforeAll (testEnv <$> liftIO setupEnv) $ do
  describe "MonadBaseControl IO App instance" $ do
    it "works normally" $ \env -> do
      runExceptT (runApp env testControl) `shouldReturn` (Right ())

    it "doesn't handle exception" $ \env -> do
      runExceptT (runApp env testControlException) `shouldThrow` isUserError

    it "handles servant err" $ \env -> do
      Left e <- runExceptT (runApp env (throwError err401))
      errHTTPCode e `shouldBe` 401

    -- fixme: MonadBaseControl instance should be fixed to make this pass
    xit "handles servant err in base" $ \env -> do
      Left e <- runExceptT (runApp env testControlServantErr)
      errHTTPCode e `shouldBe` 401
