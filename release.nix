{ nixpkgs }:

let
  pkgs = nixpkgs {};

  jobset = import ./default.nix { inherit pkgs; };

in
  jobset
