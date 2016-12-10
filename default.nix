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

  glualint-lib-src = nixpkgs.fetchgit {
    url = "https://github.com/FPtje/GLuaFixer.git";
    rev = "ff96c8e0ede9965c3fa9c16708df380870732685";
    sha256 = "0kii6jcqzgrxqnsw6hvhxaqrvxklhbrb7mvkb4an6zkidrg3k8fa";
  };

  glualint-lib = callPackage glualint-lib-src { };

  glualint-web-styles = callPackage ./styles { };

  drv = { mkDerivation
    , base
    , glualint-lib
    , stdenv
    , ghcjs-base
    , ghcjs-ffiqq
    , ghcjs-dom
    , containers
    , dependent-sum
    , reflex
    , reflex-dom
    , safe
    , transformers
    , uu-parsinglib
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
        dependent-sum
        ghcjs-base
        ghcjs-ffiqq
        ghcjs-dom
        glualint-lib
        reflex
        reflex-dom
        safe
        transformers
        uu-parsinglib
      ];
      description = "Clientside web version of glualint";
      license = stdenv.lib.licenses.gpl2;

      postInstall = ''
        echo "Generating CSS file"
        ${glualint-web-styles}/bin/glualint-web-styles > $out/bin/glualint-web.jsexe/styles.css
      '';
    };
in

callPackage drv { inherit glualint-lib; }
