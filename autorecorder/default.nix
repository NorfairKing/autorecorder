{ mkDerivation, aeson, async, autodocodec, autodocodec-yaml, base
, bytestring, conduit, containers, directory, dirforest, lib
, normaldistribution, optparse-applicative, path, path-io, random
, stm, text, time, typed-process, unix, unliftio, yaml
}:
mkDerivation {
  pname = "autorecorder";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async autodocodec autodocodec-yaml base bytestring conduit
    containers directory dirforest normaldistribution
    optparse-applicative path path-io random stm text time
    typed-process unix unliftio yaml
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/NorfairKing/autorecorder#readme";
  license = lib.licenses.mit;
  mainProgram = "autorecorder";
}
