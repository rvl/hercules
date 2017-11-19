module Hercules.Input.NixHelper where

import System.FilePath


defaultDataPath :: FilePath
defaultDataPath = "/var/lib/hercules"

-- | Get SCM cache dir given Hercules data path
scmCacheDir :: FilePath -> FilePath
scmCacheDir = (</> "scm")

{-
sub getGCRootsDir {
    my $config = getHydraConfig();
    my $dir = $config->{gc_roots_dir};
    unless (defined $dir) {
        die unless defined $ENV{LOGNAME};
        $dir = ($ENV{NIX_STATE_DIR} || "/nix/var/nix" ) . "/gcroots/per-user/$ENV{LOGNAME}/hydra-roots";
    }
    mkpath $dir if !-e $dir;
    return $dir;
}
-}
