{ nixpkgsFunc ? import <nixpkgs>
, compiler ? "ghc802"
}:

let
  nixpkgs = nixpkgsFunc { };

  callPackage = nixpkgs.haskell.packages.${compiler}.callPackage;

in rec {
  wamp-common = callPackage ./wamp-common { };

  wamp-client = callPackage ./wamp-client {
    wamp-common = wamp-common;
  };

  wamp-router = callPackage ./wamp-router {
    wamp-common = wamp-common;
  };

  wamp = callPackage ./wamp {
    wamp-common = wamp-common;
    wamp-router = wamp-router;
  };
}
