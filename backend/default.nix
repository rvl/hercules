{ pkgs ? (import ../pkgs.nix) {}, src ? ./. }:

with (import ../haskell-packages.nix) {inherit pkgs;};
with haskellPackages; 
with pkgs;

haskellPackageGen {
  extraEnvPackages = [ opaleye-gen postgresql ];
  override = attrs: {
    testToolDepends = attrs.testToolDepends or [] ++ [ postgresql ];
    buildInputs = attrs.buildInputs ++ [ postgresql ];
    # https://github.com/hspec/hspec/issues/232
    checkPhase = "./Setup test --show-details=direct";
  };
} src
