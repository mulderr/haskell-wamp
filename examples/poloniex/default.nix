{ mkDerivation, aeson, async, base, stdenv, unordered-containers
, wamp-client, wamp-common, websockets, wuss
}:
mkDerivation {
  pname = "poloniex";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base unordered-containers wamp-client wamp-common
    websockets wuss
  ];
  license = stdenv.lib.licenses.mit;
}
