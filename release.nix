{
  nixpkgs  ? import <nixos-unstable>
, compiler ? "ghc822"
}:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              kafka-device      = haskellPackagesNew.callPackage ../kafka-device/default.nix { };
              kafka-device-glut = haskellPackagesNew.callPackage               ./default.nix { };
            };
          };
        };
      };
    };
  };
  pkgs = nixpkgs { inherit config; };
in
  {
    kafka-device-glut = pkgs.haskell.packages."${compiler}".kafka-device-glut;
  }
