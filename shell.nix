{ pkgs ? import <nixpkgs> {} }:

let
  ghcVersion = "ghc865";

  ghc = pkgs.haskell.packages."${ghcVersion}".ghcWithPackages (p: with p; [
    aeson
    directory
    filepath
    optparse-applicative
    postgresql-simple
    yaml
  ]);

in
pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
    cabal-install
  ];
}