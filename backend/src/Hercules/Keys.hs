{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hercules.Keys
  ( loadKeyFile
  , loadGitHubKey
  ) where

import Data.ByteString.Extra           as BS (readFileEither, ByteString)
import qualified Data.ByteString.Char8 as S8
import Data.Monoid
import Data.Maybe
import Say
import OpenSSL.PEM
import OpenSSL.EVP.PKey
import OpenSSL.RSA
import Crypto.JOSE.JWK (JWK, fromRSA)

import qualified Crypto.PubKey.RSA.Types as C


-- fixme: use mtl instead of silly Maybe stuff
-- fixme: consider a securemem type instead of bytestring

loadKeyFile :: Maybe FilePath -> IO (Maybe ByteString)
loadKeyFile Nothing = return Nothing
loadKeyFile (Just f) = readFileEither f >>= \case
  Right k -> return . Just . firstLine $ k
  Left e -> do
      -- fixme: need to exit instead of ignoring empty key
      sayErrString ("Failed to open " <> f <> ": " <> e)
      return Nothing
firstLine :: ByteString -> ByteString
firstLine = S8.takeWhile (\c -> c /= '\n' && c /= '\r')

loadPEM :: ByteString -> IO (Maybe C.PrivateKey)
loadPEM = fmap (fmap openSSLtoCryptonite) . loadPEM'

loadPEM' :: ByteString -> IO (Maybe RSAKeyPair)
loadPEM' k = do
  key <- readPrivateKey (S8.unpack k) PwNone
  return (toKeyPair key)

openSSLtoCryptonite :: RSAKeyPair -> C.PrivateKey
openSSLtoCryptonite k = C.PrivateKey
  (C.PublicKey (rsaSize k) (rsaN k) (rsaE k))
  (rsaD k) (rsaP k) (rsaQ k)
  (opt (rsaDMP1 k)) (opt (rsaDMQ1 k)) (opt (rsaIQMP k))
  where opt = fromMaybe 0

loadGitHubKey :: Maybe FilePath -> IO (Maybe JWK)
loadGitHubKey mf = loadKeyFile mf >>= \case
  Nothing -> return Nothing
  Just k -> fmap fromRSA <$> loadPEM k
