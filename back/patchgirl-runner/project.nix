{ mkDerivation, aeson, base, bytestring, case-insensitive
, connection, containers, dhall, hpack, hspec, http-client
, http-client-tls, http-conduit, http-types, lens, mtl
, patchgirl-web, prometheus-client, prometheus-metrics-ghc, say
, servant-client, servant-server, stdenv, stm, utf8-string, uuid
, wai, wai-cors, wai-middleware-prometheus, warp
}:
mkDerivation {
  pname = "patchgirl-runner";
  version = "0.0.0";
  src = ./patchgirl-runner;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring case-insensitive connection containers dhall
    http-client http-client-tls http-conduit http-types lens mtl
    patchgirl-web prometheus-client prometheus-metrics-ghc say
    servant-server stm utf8-string uuid wai wai-cors
    wai-middleware-prometheus warp
  ];
  testHaskellDepends = [
    aeson base bytestring case-insensitive connection containers dhall
    hspec http-client http-client-tls http-conduit http-types lens mtl
    patchgirl-web prometheus-client prometheus-metrics-ghc say
    servant-client servant-server stm utf8-string uuid wai wai-cors
    wai-middleware-prometheus warp
  ];
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
