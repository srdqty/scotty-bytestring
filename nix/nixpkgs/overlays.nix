{ compiler }:
let
  haskellOverlay = (self: super: {
    haskellPackages = super.haskell.packages."${compiler}".override {
      overrides = (new: old: {
      });
    };
  });

  nonHaskellOverlay = (self: super: {
  });
in
  [nonHaskellOverlay haskellOverlay]
