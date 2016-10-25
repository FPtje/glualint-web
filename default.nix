{pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv, lib ? pkgs.lib}:
with pkgs.haskell.packages.ghcjs;
let
  glualint-lib-src = pkgs.fetchgit {
    url = "https://github.com/FPtje/GLuaFixer.git";
    rev = "c6308c2155c83a26218acab407652f7f5c3bc390";
    sha256 = "1jnv65mlhqda8awfy00hn0d35h4xyyw05kii8yaza3yd45dgwxj1";
  };

  glualint-lib = callPackage glualint-lib-src { };

  drv = { mkDerivation, base, glualint-lib, stdenv, ghcjs-base
    , containers
    , reflex
    , reflex-dom
    , safe
    }:
    mkDerivation {
      pname = "glualint-web";
      version = "0.1.0.0";
      src = ./.;
      isLibrary = false;
      isExecutable = true;
      executableHaskellDepends = [
        base
        containers
        ghcjs-base
        glualint-lib
        reflex
        reflex-dom
        safe
      ];
      description = "Clientside web version of glualint";
      license = stdenv.lib.licenses.gpl2;
    };
in

callPackage drv { inherit glualint-lib; }
