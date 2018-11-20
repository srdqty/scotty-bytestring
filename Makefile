PROJECT_NAME?=scotty-bytestring

GHC_COMPILER?=ghc844

NIXPKGS_OWNER?=NixOS
NIXPKGS_REPO?=nixpkgs
NIXPKGS_REV?=6141939d6e0a77c84905efd560c03c3032164ef1

.PHONY: build
build: default.nix scotty-bytestring.cabal
	nix-shell --pure nix \
		--attr devel \
		--arg nixpkgs '(import nix/nixpkgs { compiler = "$(GHC_COMPILER)"; })' \
		--run "cabal build"

.PHONY: run
run: default.nix scotty-bytestring.cabal
	nix-shell --pure nix \
		--attr devel \
		--arg nixpkgs '(import nix/nixpkgs { compiler = "$(GHC_COMPILER)"; })' \
		--run "cabal run"

.PHONY: test
test: default.nix scotty-bytestring.cabal
	nix-shell --pure nix \
		--attr devel \
		--arg nixpkgs '(import nix/nixpkgs { compiler = "$(GHC_COMPILER)"; })' \
		--run "cabal test --show-details=direct"

scotty-bytestring.cabal: package.yaml
	nix-shell --pure nix/scripts/generate-cabal-file.nix \
		--arg nixpkgs '(import nix/nixpkgs { compiler = "$(GHC_COMPILER)"; })'

default.nix: package.yaml scotty-bytestring.cabal
	nix-shell --pure nix/scripts/generate-default-nix-file.nix \
		--arg nixpkgs '(import nix/nixpkgs { compiler = "$(GHC_COMPILER)"; })'

.PHONY: nix-shell
nix-shell: default.nix scotty-bytestring.cabal
	nix-shell nix \
		--attr shell \
		--arg nixpkgs '(import nix/nixpkgs { compiler = "$(GHC_COMPILER)"; })'

.PHONY: nix-build
nix-build: default.nix scotty-bytestring.cabal
	nix-build nix \
		--attr full \
		--arg nixpkgs '(import nix/nixpkgs { compiler = "$(GHC_COMPILER)"; })'

.PHONY: nix-run
nix-run: nix-build
	./result/bin/scotty-bytestring

.PHONY: update-nixpkgs
update-nixpkgs:
	nix-shell --pure nix/scripts/generate-nixpkgs-json.nix \
		--argstr owner $(NIXPKGS_OWNER) \
		--argstr repo $(NIXPKGS_REPO) \
		--argstr rev $(NIXPKGS_REV)

.PHONY: available-ghc-versions
available-ghc-versions:
	nix-shell nix/scripts/available-ghc-versions.nix \
		--arg nixpkgs '(import nix/nixpkgs { compiler = "$(GHC_COMPILER)"; })'

.PHONY: change-project-name
change-project-name: clean
	find . -type f -not -path './.git/*' -not -name '*.swp' \
		| xargs -r sed -i -e "s/scotty-bytestring/$(PROJECT_NAME)/g"

.PHONY: clean
clean:
	rm -f default.nix
	rm -f scotty-bytestring.cabal
	rm -rf dist
	rm -rf dist-newstyle
	rm -f .ghc.environment.x86_64-linux-8.4.4
	rm -f result
	rm -rf .stack-work
