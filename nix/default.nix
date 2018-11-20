{ nixpkgs }:
let
  base-package = nixpkgs.haskellPackages.callPackage ./.. {};
  hlib = nixpkgs.haskell.lib;
in
  rec {
    static-executable = hlib.justStaticExecutables base-package;
    full = hlib.doCheck (hlib.doBenchmark base-package);
    devel = full.env.overrideAttrs (old: rec {
      buildInputs = old.buildInputs ++ [nixpkgs.haskellPackages.cabal-install];
    });
    shell = devel.overrideAttrs (old: rec {
      shellHook = old.shellHook + builtins.readFile ./bash-prompt.sh;
      buildInputs = old.buildInputs ++ [];
    });
  }
