module Hercules.Util
  ( sha256Hex
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteArray.Encoding as B (Base (..), convertToBase)
import Crypto.Hash

sha256Hex :: Show a => a -> String
sha256Hex a = BS8.unpack $ B.convertToBase B.Base16 sha256
  where sha256 = hash (BS8.pack $ show a) :: Digest SHA256
