{ pkgs ? import <nixpkgs> {} }:

let
hs = pkgs.haskellPackages.haskellSrc2nix {
  name = "apidoc-cli";
  src = ./.;
};
haskellPackages = pkgs.haskellPackages.override {
  overrides = se: su: {
    apidoc-th = se.callPackage ((import ../apidoc-th/default.nix { inherit pkgs; }).hs) {};
  };
};
in rec {
  inherit hs;
  pkg = haskellPackages.callPackage hs {};
  env = pkg.env.overrideAttrs (old: {
    buildInputs = with haskellPackages; [
      hlint stylish-haskell ghc-mod cabal-install
    ];
  });
}
