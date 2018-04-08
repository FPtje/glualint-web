{pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv, lib ? pkgs.lib}:
let
  # Pin nixpkgs version down by default, but allow building with another version
  nixpkgs = import (pkgs.fetchgit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "af0fec6d0a3e28c815e38296f3758e7d0916eba9";
    sha256 = "0knbmva5bmilhz4w3xi55dg22m7g44viawxa5n5x228av3bcmy5i";
  }) {};
in
with nixpkgs.haskell.packages.ghcjs;
let

  glualint-lib-src =
    nixpkgs.fetchgit {
      url = "https://github.com/FPtje/GLuaFixer.git";
      rev = "86fc9b89369638641861a473aa0e9c66ee0a16a4";
      sha256 = "1vygzpqq6jci669sf40xh6db2b061rgbhwbrccacdbn10x00bnrr";
    };

  miso-drv =
    { mkDerivation, aeson, base, bytestring, containers, fetchgit
    , ghcjs-base, http-api-data, http-types, network-uri, QuickCheck
    , quickcheck-instances, scientific, servant, stdenv, text
    , transformers, unordered-containers, vector
    }:
    mkDerivation {
      pname = "miso";
      version = "0.18.0.0";
      src = fetchgit {
        url = "https://github.com/dmjio/miso.git";
        sha256 = "0i398cpk0vmmia7nrbgpqzxik96rp5rkgm8kv27r9pxyrgkdwfm8";
        rev = "08526ed863124d2feacf31730a7a17bf26d43e28";
      };
      isLibrary = true;
      isExecutable = true;
      libraryHaskellDepends = [
        aeson base bytestring containers ghcjs-base http-api-data
        http-types network-uri scientific servant text transformers
        unordered-containers vector
      ];
      executableHaskellDepends = [
        aeson base bytestring containers ghcjs-base http-api-data
        http-types network-uri QuickCheck quickcheck-instances scientific
        servant text transformers unordered-containers vector
      ];
      homepage = "http://github.com/dmjio/miso";
      description = "A tasty Haskell front-end framework";
      license = stdenv.lib.licenses.bsd3;
    };

  miso = callPackage miso-drv {};

  glualint-lib = callPackage glualint-lib-src { };

  glualint-web-styles = callPackage ./styles { };

  drv = { mkDerivation, base, ghcjs-base, glualint-lib, lens, miso, stdenv
    , uu-parsinglib
    }:
    mkDerivation {
      pname = "glualint-web";
      version = "0.1.0.0";
      src = stdenv.lib.cleanSource ./.;
      isLibrary = false;
      isExecutable = true;
      executableHaskellDepends = [
        base ghcjs-base glualint-lib lens miso uu-parsinglib
      ];
      description = "Clientside web version of glualint";
      license = stdenv.lib.licenses.gpl2;

      postInstall = ''
        echo "Generating CSS file"
        ${glualint-web-styles}/bin/glualint-web-styles > $out/bin/glualint-web.jsexe/styles.css
      '';
    };
in

callPackage drv { inherit glualint-lib miso; }
