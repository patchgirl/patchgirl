{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module App where


import qualified Control.Monad.Except                  as Except
import qualified Control.Monad.IO.Class                as IO
import qualified Control.Monad.Reader                  as Reader
import           Data.UUID
import qualified GHC.Generics                          as Generics
import qualified GHC.Natural                           as Natural
import qualified Network.Wai.Handler.Warp              as Warp
import qualified Network.Wai.Handler.WarpTLS           as WarpTLS
import qualified Network.Wai.Middleware.Prometheus     as Prometheus
import qualified Prometheus
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
import           RequestCollection.Model
import           RequestComputation.App
import           RequestNode.App
import           RequestNode.Model
import           Servant.Auth.Server.Internal.ThrowAll (ThrowAll)
import           Session.App
import           Session.Model
import           Test


-- * api


-- ** combined


type CombinedApi auths =
  RestApi auths :<|>
  TestApi :<|>
  AssetApi

combinedApiServer :: CookieSettings -> JWTSettings -> ServerT (CombinedApi a) AppM
combinedApiServer cookieSettings jwtSettings =
  restApiServer cookieSettings jwtSettings :<|>
  testApiServer :<|>
  assetApiServer


-- ** rest


type RestApi auths =
  PRequestCollectionApi auths :<|>
  PEnvironmentApi auths :<|>
  PRequestNodeApi auths :<|>
  PRequestFileApi auths :<|>
  PRequestFolderApi auths :<|>
  PRequestComputationApi auths :<|>
  SessionApi :<|>
  PSessionApi auths :<|>
  AccountApi  :<|>
  HealthApi

restApiServer :: CookieSettings -> JWTSettings -> ServerT (RestApi a) AppM
restApiServer cookieSettings jwtSettings =
   requestCollectionApiServer
  :<|> environmentApiServer
  :<|> requestNodeApiServer
  :<|> requestFileApiServer
  :<|> requestFolderApiServer
  :<|> requestComputationApiServer
  :<|> sessionApiServer cookieSettings jwtSettings
  :<|> pSessionApiServer cookieSettings jwtSettings
  :<|> accountApiServer
  :<|> healthApiServer


-- ** request collection


type PRequestCollectionApi auths =
  Flat (Auth auths CookieSession :> RequestCollectionApi)

type RequestCollectionApi =
    "api" :> "requestCollection" :> Get '[JSON] RequestCollection

requestCollectionApiServer :: (AuthResult CookieSession -> AppM RequestCollection)
requestCollectionApiServer =
  authorizeWithAccountId getRequestCollectionHandler


-- ** environment


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


-- ** request node


type PRequestNodeApi auths =
  Flat (Auth auths CookieSession :> RequestNodeApi)

type RequestNodeApi =
  Flat (
    "api" :> "requestCollection" :> Capture "requestCollectionId" Int :> "requestNode" :> Capture "requestNodeId" UUID :> (
      -- update request node
      ReqBody '[JSON] UpdateRequestNode :> Put '[JSON] () :<|>
      -- delete request node
      Delete '[JSON] ()
    )
  )

requestNodeApiServer
  :: (AuthResult CookieSession -> Int -> UUID -> UpdateRequestNode -> AppM ())
  :<|> (AuthResult CookieSession -> Int -> UUID -> AppM ())
requestNodeApiServer =
  authorizeWithAccountId updateRequestNodeHandler
  :<|> authorizeWithAccountId deleteRequestNodeHandler


-- ** request file api


type PRequestFileApi auths =
  Flat (Auth auths CookieSession :> RequestFileApi)


type RequestFileApi =
  Flat (
    "api" :> "requestCollection" :> Capture "requestCollectionId" Int :> (
      "requestFile" :> (
          -- createRequestFile
          ReqBody '[JSON] NewRequestFile :> Post '[JSON] ()
      ) :<|>
      "rootRequestFile" :> (
          -- createRootRequestFile
          ReqBody '[JSON] NewRootRequestFile :> Post '[JSON] ()
      )
    )
  )

requestFileApiServer
  :: (AuthResult CookieSession -> Int -> NewRequestFile -> AppM ())
  :<|> (AuthResult CookieSession -> Int -> NewRootRequestFile -> AppM ())
requestFileApiServer =
  authorizeWithAccountId createRequestFileHandler
  :<|> authorizeWithAccountId createRootRequestFileHandler


-- ** request folder api


type PRequestFolderApi auths =
  Flat (Auth auths CookieSession :> RequestFolderApi)


type RequestFolderApi =
  Flat (
    "api" :> "requestCollection" :> Capture "requestCollectionId" Int :> "requestFolder" :> (
      -- create request folder
      ReqBody '[JSON] NewRequestFolder :> Post '[JSON] ()
    )
  )

requestFolderApiServer :: AuthResult CookieSession -> ServerT RequestFolderApi AppM
requestFolderApiServer =
  authorizeWithAccountId createRequestFolderHandler


-- ** request computation


type PRequestComputationApi auths =
  Flat (Auth auths CookieSession :> RequestComputationApi)

type RequestComputationApi =
  Flat (
    "api" :> "requestComputation" :> (
      ReqBody '[JSON] RequestComputationInput :> Post '[JSON] RequestComputationResult
    )
  )

requestComputationApiServer :: AuthResult CookieSession -> ServerT RequestComputationApi AppM
requestComputationApiServer =
  authorizeWithAccountId runRequestComputationHandler


-- ** session


type SessionApi =
  "api" :> "session" :> (
    "signin" :> ReqBody '[JSON] SignIn :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                                 , Header "Set-Cookie" SetCookie
                                                                 ] Session) :<|>
    "signout" :> Delete '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                          , Header "Set-Cookie" SetCookie
                                          ] Session)
  )


-- ** whoami


type PSessionApi auths =
  Auth auths CookieSession :> WhoAmiApi


type WhoAmiApi =
  "api" :> "session" :> "whoami" :> Get '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                          , Header "Set-Cookie" SetCookie
                                                          ] Session)


-- ** account

type AccountApi =
  "api" :> "account" :> (
    "signup" :> ReqBody '[JSON] SignUp :> PostNoContent '[JSON] () :<|>
    "initializePassword" :> ReqBody '[JSON] InitializePassword :> Post '[JSON] ()
  )


-- ** health


type HealthApi =
  "api" :> "health" :> Get '[JSON] AppHealth

healthApiServer :: ServerT HealthApi AppM
healthApiServer =
  getAppHealthHandler


-- ** other


type TestApi =
  Flat (
    "test" :> (
      "deleteNoContent" :> DeleteNoContent '[JSON] NoContent :<|>
      "getNotFound" :> Get '[JSON] () :<|>
      "getInternalServerError" :> Get '[JSON] ()
    )
  )

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
      combinedApiServer cookieSettings jwtSettings
    server =
      hoistServerWithContext combinedApiProxy (Proxy :: Proxy '[CookieSettings, JWTSettings]) (appMToHandler config) apiServer
  return $
    serveWithContext combinedApiProxy context server
