module Data.Aeson.Extra
  ( prefixOptions
  ) where

import Data.Aeson
import qualified Data.Aeson.Types as JSON
import Data.Aeson.Types           (camelTo2)

modifier :: String -> String
modifier = drop 1 . dropWhile (/= '_') . dropWhile (== '_') . camelTo2 '_'

prefixOptions :: JSON.Options
prefixOptions = JSON.defaultOptions { JSON.fieldLabelModifier = modifier }
