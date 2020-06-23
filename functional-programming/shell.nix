{ nixpkgs ? import <nixpkgs> { } }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  project = import ./hw1/release.nix;

in pkgs.stdenv.mkDerivation {
  name = "fp-homeworks-shell";
  buildInputs = with haskellPackages;
    project.env.nativeBuildInputs
    ++ [ cabal-install hedgehog hedgehog-fn hspec tasty tasty-hspec tasty-hedgehog ];
}
