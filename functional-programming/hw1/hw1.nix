{ mkDerivation, base, hedgehog, hedgehog-fn, mtl, split, stdenv, tasty
, tasty-hedgehog }:
mkDerivation {
  pname = "hw1";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base mtl ];
  testHaskellDepends = [ base hedgehog hedgehog-fn split tasty tasty-hedgehog ];
  homepage =
    "https://github.com/nothingelsematters/functional-programming-course/tree/master/hw1#readme";
  description = "First home howrk";
  license = stdenv.lib.licenses.mit;
}
