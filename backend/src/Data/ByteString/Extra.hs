module Data.ByteString.Extra
  ( module Data.ByteString
  , readFileMaybe
  , readFileEither
  ) where

import Control.Exception
import Data.ByteString

-- | Catch any 'IOException's and return Nothing, otherwise the file contents
readFileMaybe :: FilePath -> IO (Maybe ByteString)
readFileMaybe f = catch (Just <$> Data.ByteString.readFile f) h
  where h :: IOException -> IO (Maybe a)
        h = pure . const Nothing

readFileEither :: FilePath -> IO (Either String ByteString)
readFileEither f = mapLeft fmt <$> try (Data.ByteString.readFile f)
  where
    mapLeft :: (a -> c) -> Either a b -> Either c b
    mapLeft l = either (Left . l) Right
    fmt :: IOException -> String
    fmt = displayException
