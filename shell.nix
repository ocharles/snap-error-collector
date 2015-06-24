let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellngPackages.override {
    overrides = self: super: {
      snap-error-collector = self.callPackage ./. {};
    };
  };
in haskellPackages.snap-error-collector.env
