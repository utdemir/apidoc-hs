{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc802"
}:

let
haskellPackages = pkgs.haskell.packages.${compiler}.override {
  overrides = se: su: {
    apidoc-th =
      let drv = se.callPackage ((import ../apidoc-th/default.nix { inherit pkgs; }).hs) {};
      in  pkgs.haskell.lib.overrideCabal drv (su: {
        doCheck = false; doHaddock = false;
      });
  };
};
hs = haskellPackages.haskellSrc2nix {
  name = "apidoc-cli";
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
