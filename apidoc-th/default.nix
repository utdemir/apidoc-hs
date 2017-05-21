{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc802"
}:

let
haskellPackages = pkgs.haskell.packages.${compiler};
hs = haskellPackages.haskellSrc2nix {
  name = "apidoc-th";
  src = ./.;
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
