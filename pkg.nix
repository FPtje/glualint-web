{ mkDerivation, base, glualint-lib, stdenv, ghcjs-base ? null
, aeson, array, bytestring, containers
, directory, filemanip, filepath, ListLike, MissingH, mtl, parsec
, pretty, uu-parsinglib, uuagc, uuagc-cabal, vector, lib
}:
mkDerivation {
  pname = "glualint-web";
  version = "0.1.0.0";
  src =./.;
  isLibrary = false;
  isExecutable = true;
  # extraLibraries = [glualint-lib];
  # libraryHaskellDepends = [glualint-lib];
  executableHaskellDepends = [
    base
    ghcjs-base
    # glualint-lib
    aeson array base bytestring containers directory filemanip filepath
    ListLike MissingH mtl parsec pretty uu-parsinglib uuagc uuagc-cabal
    vector
  ];
  description = "Clientside web version of glualint";
  license = stdenv.lib.licenses.gpl2;
}
