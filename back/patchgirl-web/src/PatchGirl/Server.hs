{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module PatchGirl.Server(run, mkApp) where

import qualified GHC.Natural                       as Natural
import qualified Network.Connection                as Tls
import qualified Network.HTTP.Client.TLS           as Tls
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


import           Env
import           PatchGirl.Api
import           PatchGirl.Model
import           RequestComputation.App


run :: Mode -> IO ()
run mode = do
  putStrLn "Running web server"
  env :: Env <- createEnv mode Say.sayString ioRequestRunner
  _ <- Prometheus.register Prometheus.ghcMetrics
  let
    promMiddleware = Prometheus.prometheus $ Prometheus.PrometheusSettings ["metrics"] True True
  Warp.run (Natural.naturalToInt $ _envPort env ) =<< promMiddleware <$> mkApp env

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
    combinedApiProxy = Proxy :: Proxy (CombinedApi '[Cookie, JWT]) -- JWT is needed for the tests to run
    apiServer =
      combinedApiServer cookieSettings jwtSettings
    server =
      hoistServerWithContext combinedApiProxy (Proxy :: Proxy '[CookieSettings, JWTSettings]) (appMToHandler env) apiServer
  return $
    serveWithContext combinedApiProxy context server
