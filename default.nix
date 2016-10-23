{pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv, lib ? pkgs.lib}:
with pkgs.haskell.packages.ghcjs;
# with pkgs.haskellPackages;
let
  glualint-lib-src = pkgs.fetchgit {
    url = "https://github.com/FPtje/GLuaFixer.git";
    rev = "e10fccccf5ea1c0993676c233dc0cefead68ec47";
    sha256 = "02dwdc7p95zadmv8ymchs1vq395l92d9qqh33y42mv6cra5qbvq8";
  };


  glualint-lib = callPackage ../glualint { };

  drv = { mkDerivation, base, glualint-lib, stdenv, ghcjs-base ? null
    , aeson, array, bytestring, containers
    , directory, filemanip, filepath, ListLike, MissingH, mtl, parsec
    , pretty, uu-parsinglib, uuagc, uuagc-cabal, vector
    }:
    mkDerivation {
      pname = "glualint-web";
      version = "0.1.0.0";
      src = ./.;
      isLibrary = false;
      isExecutable = true;
      # extraLibraries = [glualint-lib];
      libraryHaskellDepends = [uuagc uuagc-cabal];
      executableHaskellDepends = [
        base
        ghcjs-base
        glualint-lib
        aeson array base bytestring containers directory filemanip filepath
        ListLike MissingH mtl parsec pretty uu-parsinglib uuagc uuagc-cabal
        vector
      ];
      description = "Clientside web version of glualint";
      license = stdenv.lib.licenses.gpl2;
    };
in

callPackage drv { inherit glualint-lib; }

# mkDerivation {
#   pname = "glualint-web";
#   version = "0.1.0.0";
#   src = ./.;
#   isLibrary = false;
#   isExecutable = true;
#   executableHaskellDepends = [
#     base
#     uuagc
#     uuagc-cabal
#     glualint-lib
#     # pkgs.haskell.compiler.ghcjs.ghcWithPackages [ glualint-lib base uuagc uuagc-cabal ghcjs-base ghcjs-dom ]
#     ghcjs-base
#     ghcjs-dom
#   ];
#   description = "Clientside web version of glualint";
#   license = stdenv.lib.licenses.gpl2;
# }
