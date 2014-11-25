{ cabal, async, monadLoops, snap }:
cabal.mkDerivation (self: {
  pname = "snap-error-collector";
  version = "1.0.0";
  src = ./.;
  buildDepends = [ async monadLoops snap ];
})
