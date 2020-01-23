{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module App where


import qualified Control.Monad.Except                  as Except
import qualified Control.Monad.IO.Class                as IO
import qualified Control.Monad.Reader                  as Reader
import qualified GHC.Generics                          as Generics
import qualified GHC.Natural                           as Natural
import qualified Network.Wai.Handler.Warp              as Warp
import qualified Network.Wai.Handler.WarpTLS           as WarpTLS
import qualified Network.Wai.Middleware.Prometheus     as Prometheus
import qualified Prometheus                            as Prometheus
import qualified Prometheus.Metric.GHC                 as Prometheus
import           Servant                               hiding (BadPassword,
                                                        NoSuchUser)
import           Servant.API.ContentTypes              (NoContent)
import           Servant.API.Flatten                   (Flat)
import           Servant.Auth.Server                   (Auth, AuthResult (..),
                                                        Cookie, CookieSettings,
                                                        IsSecure (..), JWT,
                                                        JWTSettings,
                                                        SameSite (..),
                                                        SetCookie,
                                                        cookieIsSecure,
                                                        cookieSameSite,
                                                        cookieXsrfSetting,
                                                        defaultCookieSettings,
                                                        defaultJWTSettings,
                                                        readKey,
                                                        sessionCookieName,
                                                        throwAll)

import           Account.App
import           Account.Model
import           Config
import           Environment.App
import           Health.App
import           RequestCollection.App
import           RequestComputation.App
import           RequestNode.Model
import           Servant.Auth.Server.Internal.ThrowAll (ThrowAll)
import           Session.App
import           Session.Model
import           Test


-- * api

type CombinedApi auths =
  RestApi auths :<|>
  TestApi :<|>
  AssetApi

type RestApi auths =
  PRequestCollectionApi auths :<|>
  PEnvironmentApi auths :<|>
  SessionApi :<|> PSessionApi auths :<|>
  AccountApi

type PRequestCollectionApi auths =
  Auth auths CookieSession :> RequestCollectionApi

type RequestCollectionApi =
  "api" :> "requestCollection" :> Capture "requestCollectionId" Int :> Get '[JSON] RequestCollection

type PEnvironmentApi auths =
  Flat (Auth auths CookieSession :> EnvironmentApi)

type EnvironmentApi =
  Flat (
    "api" :> "environment" :> (
      ReqBody '[JSON] NewEnvironment :> Post '[JSON] Int :<|> -- createEnvironment
      Get '[JSON] [Environment] :<|> -- getEnvironments
      Capture "environmentId" Int :> (
        ReqBody '[JSON] UpdateEnvironment :> Put '[JSON] () :<|> -- updateEnvironment
        Delete '[JSON] () :<|>  -- deleteEnvironment
        "keyValue" :> (
          ReqBody '[JSON] [NewKeyValue] :> Put '[JSON] [KeyValue] :<|> -- updateKeyValues
          Capture "keyValueId" Int :> Delete '[JSON] () -- deleteKeyValues
        )
      )
    )
  )

type SessionApi =
  "api" :> "session" :> (
    "signin" :> ReqBody '[JSON] SignIn :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                                 , Header "Set-Cookie" SetCookie
                                                                 ] Session) :<|>
    "signout" :> Delete '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                          , Header "Set-Cookie" SetCookie
                                          ] Session)
  )

type PSessionApi auths =
  Auth auths CookieSession :> "api" :> "session" :> "whoami" :> WhoAmiApi

type WhoAmiApi =
  Get '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                        , Header "Set-Cookie" SetCookie
                        ] Session)

type AccountApi =
  "api" :> "account" :> (
    "signup" :> ReqBody '[JSON] SignUp :> PostNoContent '[JSON] () :<|>
    "initializePassword" :> ReqBody '[JSON] InitializePassword :> Post '[JSON] ()
  )

type RequestNodeApi =
  Flat (
    "api" :> "requestCollection" :> Capture "requestCollectionId" Int :> "requestNode" :> Capture "requestNodeId" Int :> (
      ReqBody '[JSON] UpdateRequestNode :> Put '[JSON] NoContent
    )
  )

type RequestFileApi = Flat (
  "api" :> "requestCollection" :> Capture "requestCollectionId" Int :> "requestFile" :> (
    -- createRequestFile
    ReqBody '[JSON] NewRequestFile :> Post '[JSON] Int -- :<|>
    -- updateRequestFile
    -- Capture "requestFileId" Int :> Put '[JSON] Int
    )
  )

type RequestComputationApi =
  Flat (
    "api" :> "requestComputation" :> (
      ReqBody '[JSON] RequestComputationInput :> Post '[JSON] RequestComputationResult
    )
  )

type TestApi =
  Flat (
    "test" :> (
      "deleteNoContent" :> DeleteNoContent '[JSON] NoContent :<|>
      "getNotFound" :> Get '[JSON] () :<|>
      "getInternalServerError" :> Get '[JSON] ()
    )
  )

type HealthApi =
  "api" :> "health" :> Get '[JSON] AppHealth

type AssetApi =
  "public" :> Raw


-- * server


pSessionApiServer
  :: CookieSettings
  -> JWTSettings
  -> AuthResult CookieSession
  -> ServerT WhoAmiApi AppM
pSessionApiServer cookieSettings jwtSettings cookieSessionAuthResult =
  whoAmIHandler cookieSettings jwtSettings cookieSessionAuthResult

sessionApiServer
  :: CookieSettings
  -> JWTSettings
  -> ServerT SessionApi AppM
sessionApiServer cookieSettings jwtSettings  =
  signInHandler cookieSettings jwtSettings :<|>
  deleteSessionHandler cookieSettings

accountApiServer :: ServerT AccountApi AppM
accountApiServer =
  signUpHandler :<|>
  initializePasswordHandler

authorizeWithAccountId
  :: Servant.Auth.Server.Internal.ThrowAll.ThrowAll p
  => (Int -> p) -> AuthResult CookieSession -> p
authorizeWithAccountId f = \case
  BadPassword ->
    throwAll err402

  NoSuchUser ->
    throwAll err403

  Indefinite ->
    throwAll err405

  Authenticated cookieSession ->
    f (_cookieAccountId cookieSession)

authorize
  :: Servant.Auth.Server.Internal.ThrowAll.ThrowAll p
  => (t -> p) -> AuthResult t -> p
authorize f = \case
  BadPassword ->
    throwAll err402

  NoSuchUser ->
    throwAll err403

  Indefinite ->
    throwAll err405

  Authenticated cookieSession ->
    f cookieSession

requestCollectionApiServer :: AuthResult CookieSession -> ServerT RequestCollectionApi AppM
requestCollectionApiServer =
  authorize getRequestCollectionHandler

environmentApiServer
  :: (AuthResult CookieSession -> NewEnvironment -> AppM Int)
  :<|> ((AuthResult CookieSession -> AppM [Environment])
  :<|> ((AuthResult CookieSession -> Int -> UpdateEnvironment -> AppM ())
  :<|> ((AuthResult CookieSession -> Int -> AppM ())
  :<|> ((AuthResult CookieSession -> Int -> [NewKeyValue] -> AppM [KeyValue])
  :<|> (AuthResult CookieSession -> Int -> Int -> AppM ())))))
environmentApiServer =
  authorizeWithAccountId createEnvironmentHandler
  :<|> authorizeWithAccountId getEnvironmentsHandler
  :<|> authorizeWithAccountId updateEnvironmentHandler
  :<|> authorizeWithAccountId deleteEnvironmentHandler
  :<|> authorizeWithAccountId updateKeyValuesHandler
  :<|> authorizeWithAccountId deleteKeyValueHandler


{-
protectedApiServer :: AuthResult CookieSession -> ServerT ProtectedApi AppM
protectedApiServer = \case
  BadPassword ->
    throwAll err402

  NoSuchUser ->
    throwAll err403

  Indefinite ->
    throwAll err405

  Authenticated _ ->
    requestCollectionApi :<|>
    requestNodeApi :<|>
    requestFileApi :<|>
    environmentApi :<|>
    runRequestComputationHandler :<|>
    getAppHealth

  where
    requestCollectionApi =
      getRequestCollectionHandler
    requestNodeApi =
      updateRequestNodeHandler
    requestFileApi =
      createRequestFile
    environmentApi =
      createEnvironmentHandler :<|>
      getEnvironmentsHandler :<|>
      updateEnvironmentHandler :<|>
      deleteEnvironmentHandler :<|>
      updateKeyValuesHandler :<|>
      deleteKeyValueHandler
-}

testApiServer :: ServerT TestApi AppM
testApiServer =
  deleteNoContentHandler :<|>
  getNotFoundHandler :<|>
  getInternalServerErrorHandler

assetApiServer :: ServerT AssetApi AppM
assetApiServer =
  serveDirectoryWebApp "../public"


-- * model


newtype AppM a =
  AppM { unAppM :: Except.ExceptT ServerError (Reader.ReaderT Config IO) a }
  deriving ( Except.MonadError ServerError
           , Reader.MonadReader Config
           , Functor
           , Applicative
           , Monad
           , IO.MonadIO
           , Generics.Generic
           )

appMToHandler
  :: Config
  -> AppM a
  -> Handler a
appMToHandler config r = do
  eitherErrorOrResult <- IO.liftIO $ flip Reader.runReaderT config . Except.runExceptT . unAppM $ r
  case eitherErrorOrResult of
    Left error   -> throwError error
    Right result -> return result

-- * app


run :: IO ()
run = do
  config :: Config <- importConfig
  print config
  _ <- Prometheus.register Prometheus.ghcMetrics
  let
    promMiddleware = Prometheus.prometheus $ Prometheus.PrometheusSettings ["metrics"] True True
    settings = Warp.setPort (Natural.naturalToInt $ port config) Warp.defaultSettings
    tlsOpts = WarpTLS.tlsSettings "cert.pem" "key.pem"
  WarpTLS.runTLS tlsOpts settings =<< promMiddleware <$> mkApp config

mkApp :: Config -> IO Application
mkApp config = do
  key <- readKey $ appKeyFilePath config
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
      (requestCollectionApiServer :<|>
      environmentApiServer :<|>
      (sessionApiServer cookieSettings jwtSettings :<|> pSessionApiServer cookieSettings jwtSettings :<|> accountApiServer)) :<|>
      testApiServer :<|>
      assetApiServer
    server =
      hoistServerWithContext combinedApiProxy (Proxy :: Proxy '[CookieSettings, JWTSettings]) (appMToHandler config) apiServer
  return $
    serveWithContext combinedApiProxy context server
