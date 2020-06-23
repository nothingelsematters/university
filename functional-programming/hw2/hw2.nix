{ mkDerivation, base, Diff, directory, extra, filepath, mtl
, optparse-applicative, random, split, stdenv, strict, text, time, transformers
, unordered-containers }:

mkDerivation {
  pname = "hw2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    Diff
    directory
    extra
    filepath
    mtl
    optparse-applicative
    random
    split
    strict
    text
    time
    transformers
    unordered-containers
  ];
  executableHaskellDepends =
    [ base directory optparse-applicative transformers ];
  testHaskellDepends = [ base filepath mtl transformers unordered-containers ];
  homepage =
    "https://github.com/fp-ctd-itmo/hw2-nothingelsematters/tree/master#readme";
  description = "Second home howrk";
  license = stdenv.lib.licenses.mit;
}
