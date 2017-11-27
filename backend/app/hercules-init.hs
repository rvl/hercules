{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Extra as BS
import           Data.Foldable         (fold)
import qualified Data.Text             as T
import           Data.Text.Encoding    (encodeUtf8)
import           Data.Yaml             (decodeFileEither,
                                        prettyPrintParseException)
import           Options.Applicative
import           System.Exit
import           System.IO             (hPutStrLn, stderr)
import           Control.Monad         (when)
import           Data.Monoid
import Say
import Database.PostgreSQL.Simple      (Connection, close, connectPostgreSQL, withTransaction)

import Hercules.Config
import Hercules.Lib
import Hercules.Database.Hercules.Migration

main :: IO ()
main = getConfig >>= uncurry herculesInit

herculesInit :: Verbosity -> Config -> IO ()
herculesInit v Config{..} = do
  conn <- connectPostgreSQL (encodeUtf8 configDatabaseConnectionString)
  readyDatabase v conn >>= \case
    MigrationError s -> do
      sayErr ("Error migrating hercules database: " <> s)
      exitFailure
    MigrationSuccess -> when (v == Verbose) (say "Hercules database migrated.")


-- | Parse the command line options. If incorrect options are given exit with
-- 'exitFailure'.
getConfig :: IO (Verbosity, Config)
getConfig = do
  (v, f) <- execParser options
  decodeFileEither f >>= \case
    Left err -> do
      sayErrString (prettyPrintParseException err)
      exitFailure
    Right c -> pure (v, c)

options :: ParserInfo (Verbosity, FilePath)
options = info (helper <*> parser) description
  where
    parser = (,) <$> verbosity <*> configFileParser
    configFileParser = strOption (fold [ long "config"
                                       , short 'c'
                                       , metavar "FILE"
                                       , help "Configuration in Haskell syntax"
                                       ])
    verbosity = boolToVerbosity <$> switch ( long "verbose" <> short 'v' <> help "Show more information" )
    description = fold
      [ fullDesc
      , header "hercules-init"
      , progDesc "Initializes the Hercules and Hydra database"
      ]

boolToVerbosity :: Bool -> Verbosity
boolToVerbosity b = if b then Verbose else Quiet
