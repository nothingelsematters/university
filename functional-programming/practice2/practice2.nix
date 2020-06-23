{ mkDerivation, base, stdenv }:

mkDerivation {
  pname = "practice2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  homepage =
    "https://github.com/nothingelsematters/functional-programming-course/tree/master/practice2#readme";
  description = "Second practice tasks";
  license = stdenv.lib.licenses.mit;
}
