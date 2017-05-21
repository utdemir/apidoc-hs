{ pkgs ? import <nixpkgs> {} }:

let
hs = pkgs.haskellPackages.haskellSrc2nix {
  name = "apidoc-th";
  src = ./.;
};
haskellPackages = pkgs.haskellPackages;
in rec {
  inherit hs;
  pkg = haskellPackages.callPackage hs {};
  env = pkg.env.overrideAttrs (old: {
    buildInputs = with haskellPackages; [
      hlint stylish-haskell ghc-mod cabal-install
    ];
  });
}
