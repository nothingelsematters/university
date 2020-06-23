{ mkDerivation, base, stdenv }:

mkDerivation {
  pname = "hw0";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage =
    "https://github.com/nothingelsematters/functional-programming-course/tree/master/hw0#readme";
  description = "Functional programming introduction";
  license = stdenv.lib.licenses.mit;
}
