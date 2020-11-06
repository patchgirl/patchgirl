{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}

module PatchGirl.Web.Server(run, mkApp) where

import qualified GHC.Natural                       as Natural
import qualified Network.Connection                as Tls
import qualified Network.HTTP.Client.TLS           as Tls
import           Network.Wai                       (Middleware)
import qualified Network.Wai.Handler.Warp          as Warp
import qualified Network.Wai.Middleware.Prometheus as Prometheus
import qualified Prometheus
import qualified Prometheus.Metric.GHC             as Prometheus
import qualified Say
import           Servant                           hiding (BadPassword,
                                                    NoSuchUser)
import           Servant.Auth.Server               (Cookie, CookieSettings, JWT,
                                                    JWTSettings, SameSite (..),
                                                    cookieIsSecure,
                                                    cookieSameSite,
                                                    cookieXsrfSetting,
                                                    defaultCookieSettings,
                                                    defaultJWTSettings, readKey,
                                                    sessionCookieName)


import           PatchGirl.Web.Api                 (WebApi, webApiServer)
import           PatchGirl.Web.Internal.Env
import           PatchGirl.Web.Model


-- * run


run :: IO ()
run = do
  Say.sayString "Running web server for web"
  env :: Env <- createEnv Say.sayString
  _ <- Prometheus.register Prometheus.ghcMetrics
  app :: Application <- mkApp env
  let promMiddleware :: Middleware = Prometheus.prometheus $ Prometheus.PrometheusSettings ["metrics"] True True
  Warp.run (Natural.naturalToInt $ _envPort env ) (promMiddleware app)


-- * mk app


mkApp :: Env -> IO Application
mkApp env = do
  let tlsSettings = Tls.TLSSettingsSimple True False False
  -- this manager will mainly be used by RequestComputation
  Tls.setGlobalManager =<< Tls.newTlsManagerWith (Tls.mkManagerSettings tlsSettings Nothing)
  key <- readKey $ _envAppKeyFilePath env
  let
    jwtSettings = defaultJWTSettings key
    cookieSettings =
      defaultCookieSettings { cookieIsSecure = Secure
                            , cookieSameSite = AnySite
                            , cookieXsrfSetting = Nothing
                            , sessionCookieName = "JWT"
                            }
    context = cookieSettings :. jwtSettings :. EmptyContext
    webApiProxy = Proxy :: Proxy (WebApi '[Cookie, JWT]) -- JWT is needed for the tests to run
    apiServer =
      webApiServer cookieSettings jwtSettings
    server =
      hoistServerWithContext webApiProxy (Proxy :: Proxy '[CookieSettings, JWTSettings]) (appMToHandler env) apiServer
  return $
    serveWithContext webApiProxy context server
