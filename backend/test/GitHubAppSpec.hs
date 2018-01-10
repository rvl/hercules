{-# LANGUAGE OverloadedStrings #-}

module GitHubAppSpec where

import           Test.Hspec
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import Crypto.JOSE.Types (Base64Integer(..))
import Crypto.JOSE.JWK
import Control.Lens
import Data.Either

import Hercules.Keys

spec :: Spec
spec = do
  describe "Loading GitHub App private key" $ do
    it "can load the key" $ do
      Right k <- loadGitHubKey "test/data/github-app.pem"
      let RSAKeyMaterial rsa = k ^. jwkMaterial
      (rsaD <$> rsa ^. rsaPrivateKeyParameters) `shouldBe` Just testD
    it "fails to load non-existent file" $ do
      loadGitHubKey "test/does-not-exist" >>= (`shouldSatisfy` isLeft)
    it "fails to load an invalid file" $ do
      loadGitHubKey "test/GitHubAppSpec.hs" >>= (`shouldSatisfy` isLeft)

testD = Base64Integer 14165443110391845609981873865228591965102504346986287189491230779877242293804250870845821215124453616044862264181425564679646418783102266405207471786306148432863129534761277580109504062259722446786743927696162847654514506573451614013097815941558884523600349468342400986716074045841147440577925863239960935004172661704846611880780542316072049785252980823884272623029102622750121397031740733628750517317964228317473156323936626755385712415628125360108948069077580164690235201780802660746691373089031001032580364638845990891761913606228173827475309888086708571832276473640135295746713327071624032139413299560785605383721
