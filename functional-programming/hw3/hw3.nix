{ mkDerivation, array, base, criterion, deepseq, directory, extra, filepath
, hashable, hedgehog, microlens, monad-loops, random, split, stdenv, tasty
, tasty-hedgehog, text, ncurses, comonad, vector }:

mkDerivation {
  pname = "hw3";
  version = "0.1.0.0";
  src = ./.;

  doHaddock = true;
  isLibrary = true;
  libraryHaskellDepends = [
    array
    base
    directory
    extra
    filepath
    hashable
    microlens
    monad-loops
    text
    random
    comonad
    vector
  ];

  isExecutable = true;
  executableHaskellDepends = [ base random ncurses ];

  doBenchmark = true;
  benchmarkHaskellDepends = [ base criterion deepseq split random hashable ];

  testHaskellDepends = [ base tasty hedgehog tasty-hedgehog split ];

  homepage =
    "https://github.com/fp-ctd-itmo/hw3-nothingelsematters/tree/master#readme";
  description = "Third home howrk";
  license = stdenv.lib.licenses.mit;
}
