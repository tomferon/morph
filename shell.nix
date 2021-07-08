{ pkgs ? import <nixpkgs> {} }:

let
  ghcVersion = "ghc8104";

  ghc = pkgs.haskell.packages."${ghcVersion}".ghcWithPackages (p: with p; [
    directory
    filepath
    optparse-applicative
    postgresql-simple
  ]);

in
pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
    cabal-install
  ];
}
