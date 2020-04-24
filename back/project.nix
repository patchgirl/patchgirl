{ mkDerivation, aeson, aeson-qq, base, bytestring, case-insensitive
, containers, control-monad-free, cookie, dhall, elm-bridge
, email-validate, hpack, hspec, http-client, http-client-tls
, http-conduit, http-types, jose, lens, monad-loops, mtl
, postgresql-simple, prometheus-client, prometheus-metrics-ghc, say
, servant, servant-auth, servant-auth-client, servant-auth-server
, servant-client, servant-elm, servant-flatten, servant-server
, stdenv, stm, strings, text, time, transformers
, unordered-containers, utf8-string, uuid, wai
, wai-middleware-prometheus, warp, warp-tls
}:
mkDerivation {
  pname = "patchGirl";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-qq base bytestring case-insensitive containers
    control-monad-free cookie dhall elm-bridge email-validate
    http-client http-client-tls http-conduit http-types jose lens
    monad-loops mtl postgresql-simple prometheus-client
    prometheus-metrics-ghc say servant servant-auth servant-auth-client
    servant-auth-server servant-client servant-elm servant-flatten
    servant-server stm strings text time transformers
    unordered-containers utf8-string uuid wai wai-middleware-prometheus
    warp warp-tls
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-qq base bytestring case-insensitive containers
    control-monad-free cookie dhall elm-bridge email-validate
    http-client http-client-tls http-conduit http-types jose lens
    monad-loops mtl postgresql-simple prometheus-client
    prometheus-metrics-ghc say servant servant-auth servant-auth-client
    servant-auth-server servant-client servant-elm servant-flatten
    servant-server stm strings text time transformers
    unordered-containers utf8-string uuid wai wai-middleware-prometheus
    warp warp-tls
  ];
  testHaskellDepends = [
    aeson aeson-qq base bytestring case-insensitive containers
    control-monad-free cookie dhall elm-bridge email-validate hspec
    http-client http-client-tls http-conduit http-types jose lens
    monad-loops mtl postgresql-simple prometheus-client
    prometheus-metrics-ghc say servant servant-auth servant-auth-client
    servant-auth-server servant-client servant-elm servant-flatten
    servant-server stm strings text time transformers
    unordered-containers utf8-string uuid wai wai-middleware-prometheus
    warp warp-tls
  ];
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}