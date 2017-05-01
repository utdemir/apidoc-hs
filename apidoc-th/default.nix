{ pkgs ? import <nixpkgs> {} }:

let
nixfile = pkgs.runCommand "cabal2nix"
  { buildInputs = [ pkgs.cabal2nix ]; } ''
  ln -s ${./apidoc-th.cabal} .
  cabal2nix . > "$out"
  '';
fun = import "${nixfile}";
haskellPackages = pkgs.haskellPackages;
in rec {
  hs  = fun;
  pkg = (pkgs.haskellPackages.callPackage fun {}).overrideDerivation (super: {
    src = ./.;
  });
  env = pkg.env.overrideAttrs (old: {
    buildInputs = with haskellPackages; [
      hlint stylish-haskell ghc-mod cabal-install
    ];
  });
}
