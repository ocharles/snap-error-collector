{ mkDerivation, async, base, containers, monad-loops
, MonadCatchIO-transformers, snap, stdenv, stm, time, transformers
}:
mkDerivation {
  pname = "snap-error-collector";
  version = "1.1.0";
  src = ./.;
  buildDepends = [
    async base containers monad-loops MonadCatchIO-transformers snap
    stm time transformers
  ];
  homepage = "http://github.com/ocharles/snap-error-collector";
  description = "Collect errors in batches and dispatch them";
  license = stdenv.lib.licenses.bsd3;
}
