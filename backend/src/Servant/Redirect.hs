{-# LANGUAGE OverloadedStrings #-}

module Servant.Redirect
  ( redirect
  , redirectBS
  ) where

import Control.Monad.Except
import Data.ByteString
import URI.ByteString
import Servant.Server

import Hercules.ServerEnv

redirectBS :: ByteString -> App a
redirectBS uri = throwError err303 { errHeaders = [("Location", uri)] }

redirect :: URI -> App a
redirect = redirectBS . serializeURIRef'
