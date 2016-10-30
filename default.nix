{pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv, lib ? pkgs.lib}:
with pkgs.haskell.packages.ghcjs;
let
  glualint-lib-src = pkgs.fetchgit {
    url = "https://github.com/FPtje/GLuaFixer.git";
    rev = "ff649cdbdfa9283f9bf106e2c1b0f59fc3c4bacb";
    sha256 = "0crbqmry0x15qa6ifq3rpajr0s29zvf9ah9jbnfy9kkhss5zzlag";
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
