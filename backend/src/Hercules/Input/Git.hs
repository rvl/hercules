{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Hercules.Input.Git
  ( fetchInputGit
  , FetchInputGit(..)
  , fetchRepoGit
  , getBuildSpec
  , fetchPullRequest
  , CloneDepth(..)
  ) where

import Network.URI (URI)
import Data.Text (Text)
import qualified Data.Text as T
import System.Process.Typed
import GHC.Generics
import System.FilePath
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Lock.FLock (withLock, Block(..), SharedExclusive(..))
import System.Exit (ExitCode(..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteArray.Encoding as B (Base (..), convertToBase)
import Data.Text.Encoding (decodeUtf8)
import Data.Default
import Data.Time.Clock (NominalDiffTime)
import Crypto.Hash
import Control.Monad
import Text.Regex.PCRE
import Safe
import Data.Maybe (fromMaybe)
import Data.Monoid
import Say

import Hercules.Input.NixHelper
import Nix.Store

-- fixme: figure out how this result is used and trim accordingly
data FetchInputGit = FetchInputGit
  { fetchGitURI :: URI
  , fetchGitStorePath :: FilePath -- fixme: is there a nix store path type?
  , fetchGitSHA256Hash :: Text
  , fetchGitRevision :: Text
  , fetchGitRevCount :: Int
  , fetchGitTag :: Text
  , fetchGitShortRev :: Text
  } deriving (Show, Eq, Generic)

data FetchInputGitConfig = FetchInputGitConfig
  { fetchGitConfigTimeout :: NominalDiffTime
  } deriving (Show, Eq, Generic)

instance Default FetchInputGitConfig where
  def = FetchInputGitConfig (fromInteger 60)

data CloneDepth = CloneShallow | CloneDeep deriving (Show, Eq)
data CloneType = CloneNormal | CloneBare deriving (Show, Eq)

fetchInputGit :: URI -- ^ Git remote clone URI
              -> CloneDepth -- ^ Shallow or full clone
              -> Maybe Text -- ^ Branch name
              -> IO FetchInputGit
fetchInputGit uri depth branch = do
  let cfg = def :: FetchInputGitConfig -- fixme: get config by project
      branch' = fromMaybe "master" branch
  clonePath <- createClonePath uri
  withCloneLock clonePath $ do
    exists <- doesDirectoryExist clonePath
    when (not exists) $ initClone uri CloneNormal clonePath

    updateBranch (T.unpack branch') clonePath

    Just revision <- getRevision branch' clonePath -- fixme: error handling

    -- Here the perl script checks CachedGitInputs for revision
    -- and then adds a temp gcroot on the storepath if it exists.
    -- In event of cache miss, it updates the CacheGitInputs table.

    sayErr $ "Checking out Git branch " <> branch' <> " from " <> (T.pack $ show uri)
    Just (sha256, storePath) <- addCloneToStore depth revision clonePath

    addTempRoot storePath

    revCount <- getRevCount revision clonePath
    gitTag <- getGitTag revision clonePath
    shortRev <- getShortRev revision clonePath

    return $ FetchInputGit uri storePath sha256 revision revCount gitTag shortRev

-- fixme: use some config object
gitCacheDir :: FilePath
gitCacheDir = scmCacheDir defaultDataPath </> "git"

uriHash :: URI -> FilePath
uriHash uri = BS8.unpack $ B.convertToBase B.Base16 sha256
  where sha256 = hash (BS8.pack $ show uri) :: Digest SHA256

-- | Return a path for cloning a repo branch.
-- Ensures that the parent directory exists.
-- fixme: probably need the branch name in the hash
createClonePath :: URI -> IO FilePath
createClonePath uri = do
  createDirectoryIfMissing True gitCacheDir
  return (gitCacheDir </> uriHash uri)

-- | Return a path for the full bare clone of a repo.
createRepoPath :: URI -> IO FilePath
createRepoPath uri = do
  let repoDir = gitCacheDir </> "repos"
  createDirectoryIfMissing True repoDir
  return (repoDir </> uriHash uri <.> "git")

withCloneLock :: FilePath -> IO a -> IO a
withCloneLock dir = withLock (dir <.> "lock") Exclusive Block

initClone :: URI -> CloneType -> FilePath -> IO ()
initClone uri t clonePath = do
  runProcess_ (proc "git" $ ["init"] ++ if t == CloneBare then ["--bare"] else [] ++ [clonePath])
  runProcess_ (git ["remote", "add", "origin", "--", show uri] clonePath)

updateBranch :: String -> FilePath -> IO ()
updateBranch branch clonePath = gitFetch [] >> gitFetch [spec]
  where
    -- fixme: set timeout on fetches
    gitFetch bs = runProcess_ (git (["fetch", "-fu", "origin"] ++ bs) clonePath)
    branch' = if isHash branch then "_hydra_tmp" else branch
    spec = concat ["+", branch, ":", branch']

-- | Updates all refs for a bare repo.
updateRepo :: FilePath -> IO ()
updateRepo clonePath = runProcess_ (git ["fetch", "-f", "origin"] clonePath)

getRevision :: Text -> FilePath -> IO (Maybe Text)
getRevision branch clonePath | isHash (T.unpack branch) = pure (Just branch)
                             | otherwise = doRevParse
  where
    doRevParse = parse . fst <$> readProcess_ (git ["rev-parse", T.unpack branch] clonePath)
    parse out = headMay (BL8.lines out) >>= hashText . BL8.unpack
    hashText h | isHash h = Just (T.pack h)
               | otherwise = Nothing

isHash :: String -> Bool
isHash h = length h == 40 && h =~ ("^[0-9a-f]+$" :: String)

-- | This adds the clone to the nix store by running nix-prefetch-git on it.
-- Hydra Perl contains a comment which says don't use nix-prefetch-git.
addCloneToStore :: CloneDepth -> Text -> FilePath -> IO (Maybe (Text, FilePath))
addCloneToStore depth revision clonePath = do
  let
    env = [ ("NIX_HASH_ALGO", "sha256")
          , ("PRINT_PATH", "1")
          ] ++ if depth == CloneDeep then deepEnv else shallowEnv
    -- 1. Checked out code often wants to be able to run `git
    -- describe', e.g., code that uses Gnulib's `git-version-gen'
    -- script.  Thus, we leave `.git' in there.
    -- 2. Ask for a "deep clone" to allow "git describe" and similar
    -- tools to work.  See
    -- http://thread.gmane.org/gmane.linux.distributions.nixos/3569
    -- for a discussion.
    deepEnv    = [ ("NIX_PREFETCH_GIT_LEAVE_DOT_GIT", "1")
                 , ("NIX_PREFETCH_GIT_DEEP_CLONE", "1") ]
    shallowEnv = [ ("NIX_PREFETCH_GIT_LEAVE_DOT_GIT", "0")
                 , ("NIX_PREFETCH_GIT_DEEP_CLONE", "") ]
    cfg = setEnv env $ proc "nix-prefetch-git" [clonePath, T.unpack revision]
  (out, _) <- readProcess_ cfg
  return $ parsePrefetch out

-- | Parses the standard output of nix-prefetch-git and
-- returns a sha256 hash and nix store path.
parsePrefetch :: ByteString -> Maybe (Text, FilePath)
parsePrefetch out = parse (BL8.split ' ' out)
  where
    parse (s:p:_) = Just (decodeUtf8 (BL8.toStrict s), BL8.unpack p)
    parse _ = Nothing

getRevCount :: Text -> FilePath -> IO Int
getRevCount r p = parseInt <$> grabGit ["rev-list", "--count"] r p
  where parseInt = fromMaybe 0 . readMay . T.unpack

getGitTag :: Text -> FilePath -> IO Text
getGitTag = grabGit ["describe", "--always"]

getShortRev :: Text -> FilePath -> IO Text
getShortRev = grabGit ["rev-parse", "--short"]

grabGit :: [Text] -> Text -> FilePath -> IO Text
grabGit cmd rev clonePath = grab . fst <$> readProcess_ (git args clonePath)
  where
    args = map T.unpack (cmd ++ [rev])
    grab = first . T.lines . decodeUtf8 . BL8.toStrict
    first [] = ""
    first (x:_) = x


-- | Makes a full bare clone of a git repo. The destination directory
-- will be created if it doesn't exist.
fetchRepoGit :: URI -- ^ Git remote clone URI
             -> IO FilePath
fetchRepoGit uri = do
  clonePath <- createRepoPath uri
  withCloneLock clonePath $ do
    exists <- doesDirectoryExist clonePath
    when (not exists) $ initClone uri CloneBare clonePath
    updateRepo clonePath
    return clonePath

-- | Fetches the pull request into an existing bare repo.
-- Returns the ref name, if successful.
fetchPullRequest :: Int -> FilePath -> IO (Either String Text)
fetchPullRequest pr clonePath = do
  let ref = "pull/" <> show pr <> "/head"
  (code, _, err) <- readProcess (git ["fetch", "-fu", "origin", ref <> ":" <> ref] clonePath)
  return $ case code of
    ExitSuccess -> Right (T.pack ref)
    ExitFailure _ -> Left (BL8.unpack err)

-- | Returns the contents of .hercules.yml for a branch, if it exists.
getBuildSpec :: Text -> FilePath -> IO (Either String ByteString)
getBuildSpec branch clonePath = do
  let ref = T.unpack $ "remotes/origin/" <> branch
  (code, out, err) <- readProcess (git ["show", ref ++ ":.hercules.yml"] clonePath)
  return $ case code of
    ExitSuccess -> Right out
    ExitFailure _ -> Left (BL8.unpack err)

-- | Process config for running git inside a repo clone
git :: [String] -> FilePath -> ProcessConfig () () ()
git args clonePath = setWorkingDir clonePath $ proc "git" args
