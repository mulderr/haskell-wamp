{ mkDerivation, aeson, base, ixset, random, stdenv
, unordered-containers, wamp-common, websockets, wuss
}:
mkDerivation {
  pname = "wamp-client";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base ixset random unordered-containers wamp-common websockets
    wuss
  ];
  license = stdenv.lib.licenses.mit;
}
