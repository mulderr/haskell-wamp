{ mkDerivation, aeson, base, bytestring, hashable, stdenv, text
, unordered-containers, vector, websockets
}:
mkDerivation {
  pname = "wamp-common";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring hashable text unordered-containers vector
    websockets
  ];
  license = stdenv.lib.licenses.mit;
}
