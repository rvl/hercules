{-# LANGUAGE ForeignFunctionInterface #-}

module Nix.Store
  ( addTempRoot
  , isValidPath
  ) where

-- fixme: need to wrap nix libstore

addTempRoot :: FilePath -> IO ()
addTempRoot _ = return ()

isValidPath :: FilePath -> IO Bool
isValidPath _ = return False
