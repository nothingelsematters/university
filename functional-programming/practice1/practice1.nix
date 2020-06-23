{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "practice1";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/nothingelsematters/functional-programming-course/tree/master/practice1#readme";
  description = "First practice task";
  license = stdenv.lib.licenses.mit;
}
