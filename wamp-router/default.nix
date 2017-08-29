{ mkDerivation, aeson, base, bytestring, ixset, random, stdenv
, unordered-containers, wamp-common, websockets
}:
mkDerivation {
  pname = "wamp-router";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring ixset random unordered-containers wamp-common
    websockets
  ];
  license = stdenv.lib.licenses.mit;
}
