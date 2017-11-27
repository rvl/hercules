{ pkgs ? (import ../pkgs.nix) {}, src ? ./. }:

with (import ../haskell-packages.nix) {inherit pkgs;};
with haskellPackages; 
with pkgs;

haskellPackageGen {
  extraEnvPackages = [ opaleye-gen postgresql ];
} src
