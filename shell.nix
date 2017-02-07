{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, containers, lifted-base
      , monad-loops, snap, stdenv, stm, time, transformers
      }:
      mkDerivation {
        pname = "snap-error-collector";
        version = "1.1.1";
        src = ./.;
        libraryHaskellDepends = [
          async base containers lifted-base monad-loops snap stm time
          transformers
        ];
        homepage = "http://github.com/ocharles/snap-error-collector";
        description = "Collect errors in batches and dispatch them";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
