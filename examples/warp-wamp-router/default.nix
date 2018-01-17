{ mkDerivation, aeson, async, base, file-embed, stdenv
, unordered-containers, wai, wai-app-static, wai-websockets, wamp
, warp, websockets
}:
mkDerivation {
  pname = "warp-wamp-router";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base file-embed unordered-containers wai wai-app-static
    wai-websockets wamp warp websockets
  ];
  license = stdenv.lib.licenses.mit;
}
