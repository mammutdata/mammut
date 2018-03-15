{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, bytestring, cryptonite
      , extensible-effects, hedgehog, lens, memory, stdenv, tasty
      , tasty-hedgehog, time
      }:
      mkDerivation {
        pname = "mammut";
        version = "0.9.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          attoparsec base bytestring cryptonite extensible-effects lens
          memory time
        ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
        homepage = "https://www.mammutdata.com";
        description = "Command-line tool to manage a Mammut vault";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
