let
    pkgs = import <nixpkgs> { inherit config; };
    config = {
        packageOverrides = pkgs: rec {
            haskellPackages = pkgs.haskellPackages.override {
                overrides = haskellPackagesNew: haskellPackagesOld: rec {
                    wamp-common = haskellPackages.callPackage ./wamp-common {};
                    warp-client = haskellPackages.callPackage ./wamp-client {};
                    wamp-router = haskellPackages.callPackage ./wamp-router {};
                    wamp = haskellPackages.callPackage ./wamp {};

                    # Examples
                    warp-wamp-router = haskellPackages.callPackage ./examples/warp-wamp-router {};
                    poloniex = haskellPackages.callPackage ./examples/poloniex {};
                };
            };
        };
    };
in {
    # Libraries - To use in REPL, call `nix-shell --attr *.env ../../default.nix` from package directory.
    # Where `*` is the package name.
    wamp-common = pkgs.haskellPackages.wamp-common;
    warp-client = pkgs.haskellPackages.warp-client;
    wamp-router = pkgs.haskellPackages.wamp-router;
    wamp = pkgs.haskellPackages.wamp;

    # Examples
    warp-wamp-router = pkgs.haskellPackages.warp-wamp-router;
    poloniex = pkgs.haskellPackages.poloniex;
}