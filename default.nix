{pkgs ? import <nixpkgs> {}}:
let
  # Pin nixpkgs version down by default, but allow building with another version
  nixpkgs = import (pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "fb94afad504cae198ef496df796e74680cb303e6";
    sha256 = "sha256-Wu5gDD9VwQVcryavYLcf1OEWU4kD76hrBcMd8NoEHT8=";
  }) {};

  ghcjs = nixpkgs.haskell.packages.ghcjs.extend (final: previous: {
    # Ghcjs is not compatible with Aeson 2
    aeson = final.aeson_1_5_6_0;

    miso = final.mkDerivation {
      pname = "miso";
      version = "1.8.2";
      src = nixpkgs.fetchFromGitHub {
        owner = "dmjio";
        repo = "miso";
        rev = "15886bd01725a551354f37cf804ebea5f9e36fa3";
        sha256 = "sha256-NjhAcTshjiVhY1DmRYdeZyBxFPR69bhlDRM28D0zkpE=";
      };
      isLibrary = true;
      isExecutable = true;
      libraryHaskellDepends = with final; [
        aeson base bytestring containers ghcjs-base http-api-data
        http-types network-uri scientific servant text transformers
        unordered-containers vector file-embed jsaddle lucid tagsoup
      ];
      executableHaskellDepends = with final; [
        aeson base bytestring containers ghcjs-base http-api-data
        http-types network-uri QuickCheck quickcheck-instances scientific
        servant text transformers unordered-containers vector
      ];
      homepage = "http://github.com/dmjio/miso";
      description = "A tasty Haskell front-end framework";
      license = nixpkgs.lib.licenses.bsd3;
    };

    # Freezes on test run
    MissingH = previous.MissingH.overrideAttrs (prev: {doCheck = false;});
    # Exits through OOM
    tagsoup = previous.tagsoup.overrideAttrs (prev: {doCheck = false;});
    # Freezes on test run
    text-short = previous.text-short.overrideAttrs (prev: {doCheck = false;});
    # Test failure
    unliftio = previous.unliftio.overrideAttrs (pref: {doCheck = false;});
    # Freezes on test run
    vector = previous.vector.overrideAttrs (prev: {doCheck = false;});
  });
in
with ghcjs;
let
  glualint-lib-src =
    nixpkgs.fetchgit {
      url = "https://github.com/FPtje/GLuaFixer.git";
      rev = "f33dcb2d81edf61e19d0f4e376b3b84609e2be24";
      sha256 = "sha256-aL5ptT+SlfEyntXr0Zd4eV/XxNG7F+HyGjrjnpXudB8=";
    };

  # Only build the library, because the executable is not compatible with GHC 8.10, which is the
  # basis of GHCJS.
  glualint-lib = nixpkgs.haskell.lib.doJailbreak ((callPackage glualint-lib-src { }).overrideAttrs (prev: {
    postPatch = ''
      ${prev.postPatch}
      awk '/executable glualint/{exit} 1' glualint.cabal > tmp
      mv tmp glualint.cabal
      rm -rf app
    '';
  }));

  glualint-web-styles = callPackage ./styles { };

  drv = { mkDerivation, base, ghcjs-base, glualint-lib, lens, miso, stdenv
    , uu-parsinglib
    }:
    mkDerivation {
      pname = "glualint-web";
      version = "0.1.0.0";
      src = nixpkgs.lib.cleanSourceWith rec {
        name = "glualint-web-src";
        src = ./.;
        filter = path: type: let
          relativePath = nixpkgs.lib.removePrefix (toString src + "/") path;
          srcWhitelist = [
            "src(/.*)?"
            "cabal\.config"
            ".*\.cabal"
            "LICENSE"
            ".*\.hs"
          ];
        in builtins.any (r: builtins.match r relativePath != null) srcWhitelist;
      };
      isLibrary = false;
      isExecutable = true;
      executableHaskellDepends = [
        base ghcjs-base glualint-lib lens miso uu-parsinglib
      ];
      description = "Clientside web version of glualint";
      license = nixpkgs.lib.licenses.gpl2;

      postInstall = ''
        echo "Generating CSS file"
        ${glualint-web-styles}/bin/glualint-web-styles > $out/bin/glualint-web.jsexe/styles.css
      '';
    };
in

callPackage drv { inherit glualint-lib miso; }
