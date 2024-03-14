{ pkgs ? import <nixpkgs> {} }:
let
  hsPkgs = nixpkgs.haskell.packages.ghc902;
  project = hsPkgs.callCabal2nix "Lime" ./. { };
in
hsPkgs.callPackage project {}
