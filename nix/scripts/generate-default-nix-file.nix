{ nixpkgs }:

nixpkgs.stdenv.mkDerivation rec {
  name = "generate-default-nix-file";

  project-root = import ../project-root.nix;

  buildInputs = [
    nixpkgs.cabal2nix
  ];

  shellHook = ''
    set -eu

    echo -e "\nRunning cabal2nix to generate default.nix ...\n"
    cabal2nix . > ${project-root}/default.nix

    exit
  '';
}
