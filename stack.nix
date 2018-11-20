let
  pkgs = import nix/nixpkgs { compiler = "ghc844"; };
in
  pkgs.haskell.lib.buildStackProject {
    name = "scotty-bytestring";
    ghc = pkgs.haskellPackages.ghc;
    buildInputs = [pkgs.zlib];
  }
