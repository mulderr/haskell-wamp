{ mkDerivation, base, stdenv, wamp-common, wamp-router }:
mkDerivation {
  pname = "wamp";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base wamp-common wamp-router ];
  description = "WAMP v2 Basic Profile";
  license = stdenv.lib.licenses.mit;
}
