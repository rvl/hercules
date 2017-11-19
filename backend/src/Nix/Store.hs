{-# LANGUAGE ForeignFunctionInterface #-}

module Nix.Store
  ( addTempRoot
  , isValidPath
  ) where

-- fixme: need to wrap nix libstore
-- i think expipiplus1 has some code for this.

addTempRoot :: FilePath -> IO ()
addTempRoot _ = return ()

isValidPath :: FilePath -> IO Bool
isValidPath _ = return False
