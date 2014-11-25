let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      snapErrorCollector = self.callPackage ./. {};
    };
  };

in pkgs.myEnvFun {
     name = haskellPackages.snapErrorCollector.name;
     buildInputs = [
       pkgs.curl
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.hscolour
       ] ++ hs.snapErrorCollector.propagatedNativeBuildInputs)))
     ];
   }
