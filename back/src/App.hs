{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module App where

import           Control.Monad.Except        (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Reader        (MonadReader, ReaderT, runReaderT)
import           Control.Monad.Trans         (liftIO)
import           GHC.Generics                (Generic)
import           GHC.Natural                 (naturalToInt)
import           Network.Wai                 hiding (Request)
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Servant                     hiding (BadPassword, NoSuchUser)
import           Servant.API.ContentTypes    (NoContent)
import           Servant.API.Flatten         (Flat)
import           Servant.Auth.Server         (Auth, AuthResult (..), Cookie,
                                              CookieSettings, IsSecure (..),
                                              JWT, JWTSettings, SameSite (..),
                                              SetCookie, cookieIsSecure,
                                              cookieSameSite, cookieXsrfSetting,
                                              defaultCookieSettings,
                                              defaultJWTSettings, generateKey,
                                              readKey, sessionCookieName,
                                              throwAll)

import           Account.App
import           Account.Model
import           AppHealth
import           Config
import           Environment.App
import           RequestCollection
import           RequestComputation.App
import           RequestNode.App
import           RequestNode.Model
import           Session.App
import           Session.Model
import           Test

-- * api

type CombinedApi auths =
  RestApi auths :<|>
  TestApi :<|>
  AssetApi

type RestApi auths =
  (Auth auths CookieSession :> ProtectedApi) :<|>
  SessionApi :<|> PSessionApi auths :<|>
  AccountApi

type SessionApi =
  "api" :> "session" :> (
    "signin" :> ReqBody '[JSON] SignIn :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                                 , Header "Set-Cookie" SetCookie
                                                                 ] Session) :<|>
    "signout" :> Delete '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                          , Header "Set-Cookie" SetCookie
                                          ] Session)
  )

type AccountApi =
  "api" :> "account" :> (
    "signup" :> ReqBody '[JSON] SignUp :> PostNoContent '[JSON] () :<|>
    "initializePassword" :> ReqBody '[JSON] InitializePassword :> Post '[JSON] ()
  )

type WhoAmiApi =
  Get '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                        , Header "Set-Cookie" SetCookie
                        ] Session)

type PSessionApi auths =
  Auth auths CookieSession :> "api" :> "session" :> "whoami" :> WhoAmiApi

type ProtectedApi =
  RequestCollectionApi :<|>
  RequestNodeApi :<|>
  RequestFileApi :<|>
  EnvironmentApi :<|>
  RequestComputationApi :<|>
  HealthApi

type RequestCollectionApi =
  "api" :> "requestCollection" :> (
    -- getRequestCollectionById
    Capture "requestCollectionId" Int :> Get '[JSON] RequestCollection
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

type EnvironmentApi =
  Flat (
    "api" :> "environment" :> (
      ReqBody '[JSON] NewEnvironment :> Post '[JSON] Int :<|> -- createEnvironment
      Get '[JSON] [Environment] :<|> -- getEnvironments
      Capture "environmentId" Int :> (
        ReqBody '[JSON] UpdateEnvironment :> Put '[JSON] () :<|> -- updateEnvironment
        Delete '[JSON] () :<|>
        KeyValueApi -- deleteEnvironment
      )
    )
  )

type RequestComputationApi =
  Flat (
    "api" :> "requestComputation" :> (
      ReqBody '[JSON] RequestComputationInput :> Post '[JSON] RequestComputationResult
    )
  )

type KeyValueApi =
  Flat (
    "keyValue" :> (
      ReqBody '[JSON] [NewKeyValue] :> Put '[JSON] [KeyValue] :<|> -- updateKeyValues
      Capture "keyValueId" Int :> Delete '[JSON] ()
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
  AppM { unAppM :: ExceptT ServerError (ReaderT Config IO) a }
  deriving ( MonadError ServerError
           , MonadReader Config
           , Functor
           , Applicative
           , Monad
           , MonadIO
           , Generic
           )

appMToHandler
  :: Config
  -> AppM a
  -> Handler a
appMToHandler config r = do
  eitherErrorOrResult <- liftIO $ flip runReaderT config . runExceptT . unAppM $ r
  case eitherErrorOrResult of
    Left error   -> throwError error
    Right result -> return result

-- * app


run :: IO ()
run = do
  config :: Config <- importConfig
  print config
  let
    settings = setPort (naturalToInt $ port config) defaultSettings
    tlsOpts = tlsSettings "cert.pem" "key.pem"
  runTLS tlsOpts settings =<< mkApp config

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
      (protectedApiServer :<|>
      (sessionApiServer cookieSettings jwtSettings :<|> pSessionApiServer cookieSettings jwtSettings :<|> accountApiServer)) :<|>
      testApiServer :<|>
      assetApiServer
    server =
      hoistServerWithContext combinedApiProxy (Proxy :: Proxy '[CookieSettings, JWTSettings]) (appMToHandler config) apiServer
  return $
    serveWithContext combinedApiProxy context server
