{ nixpkgs }:

nixpkgs.stdenv.mkDerivation rec {
  name = "generate-cabal-file";

  project-root = import ../project-root.nix;

  buildInputs = [
    nixpkgs.haskellPackages.hpack
  ];

  shellHook = ''
    set -eu

    echo -e "Running hpack to generate cabal file...\n"
    hpack ${project-root}

    exit
  '';
}
