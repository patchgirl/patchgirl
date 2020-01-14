{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module App where

import           AppHealth
import           Config
import           Control.Monad.Except        (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Reader        (MonadReader)
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Trans         (liftIO)
import           Data.ByteString.UTF8        as BSU
import           Environment.App
import           GHC.Generics                (Generic)
import           GHC.Natural                 (naturalToInt)
import           Network.Wai                 hiding (Request)
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           RequestCollection
import           RequestComputation.App
import           RequestNode.App
import           RequestNode.Model
import           Servant                     hiding (BadPassword, NoSuchUser)
import           Servant.API.ContentTypes    (NoContent)
import           Servant.API.Flatten         (Flat)
import           Servant.Auth.Server         (Auth, AuthResult (..), Cookie,
                                              CookieSettings, IsSecure (..),
                                              JWT, JWTSettings, SameSite (..),
                                              SetCookie, cookieIsSecure,
                                              cookieSameSite, cookieXsrfSetting,
                                              defaultCookieSettings,
                                              defaultJWTSettings, fromSecret,
                                              generateKey, sessionCookieName,
                                              throwAll)
import           Session.App
import           Session.Model
import           Test

-- * api

type Foo auths = Auth auths CookieSession :> Bar
type Bar = "session2" :> Get '[JSON] Int

type CombinedApi auths =
  Foo auths :<|>
  (RestApi auths) :<|>
  TestApi :<|>
  AssetApi

type RestApi auths =
  (Auth auths CookieSession :> ProtectedApi) :<|>
  SessionApi :<|> PSessionApi auths

type SessionApi =
  "session" :> (
    "signin" :> ReqBody '[JSON] Login :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                                , Header "Set-Cookie" SetCookie
                                                                ] Session) :<|>
    "signup" :> ReqBody '[JSON] SignUp :> PostNoContent '[JSON] () :<|>
    "signout" :> Delete '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                          , Header "Set-Cookie" SetCookie
                                          ] Session)
  )

type WhoAmiApi =
  Get '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                        , Header "Set-Cookie" SetCookie
                        ] Session)

type PSessionApi auths =
  Auth auths CookieSession :> "session" :> "whoami" :> WhoAmiApi

type ProtectedApi =
  RequestCollectionApi :<|>
  RequestNodeApi :<|>
  RequestFileApi :<|>
  EnvironmentApi :<|>
  RequestComputationApi :<|>
  HealthApi

type RequestCollectionApi =
  "requestCollection" :> (
    -- getRequestCollectionById
    Capture "requestCollectionId" Int :> Get '[JSON] RequestCollection
  )

type RequestNodeApi =
  Flat (
    "requestCollection" :> Capture "requestCollectionId" Int :> "requestNode" :> Capture "requestNodeId" Int :> (
      ReqBody '[JSON] UpdateRequestNode :> Put '[JSON] NoContent
    )
  )

type RequestFileApi = Flat (
  "requestCollection" :> Capture "requestCollectionId" Int :> "requestFile" :> (
    -- createRequestFile
    ReqBody '[JSON] NewRequestFile :> Post '[JSON] Int -- :<|>
    -- updateRequestFile
    -- Capture "requestFileId" Int :> Put '[JSON] Int
    )
  )

type EnvironmentApi =
  Flat (
    "environment" :> (
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
    "requestComputation" :> (
      ReqBody '[JSON] RequestComputationInput :> Post '[JSON] RequestComputationResult
    )
  )

type KeyValueApi =
  Flat (
    "keyValue" :> (
      ReqBody '[JSON] [NewKeyValue] :> Put '[JSON] [KeyValue] :<|> -- updateKeyValues
      Capture "keyValueId" Int :> (
        Delete '[JSON] ()
      )
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
  "health" :> Get '[JSON] AppHealth

type AssetApi =
  "public" :> Raw


-- * server


session3ApiServer -- :: Int
  :: AuthResult CookieSession
  -> ServerT Bar AppM
session3ApiServer = \case
  BadPassword ->
    throwAll err402

  NoSuchUser ->
    throwAll err403

  Indefinite ->
    throwAll err405

  Authenticated _ ->
    return 1

loginApiServer
  :: CookieSettings
  -> JWTSettings
  -> ServerT SessionApi AppM
loginApiServer cookieSettings jwtSettings  =
  signInHandler cookieSettings jwtSettings :<|>
  signUpHandler :<|>
  deleteSessionHandler cookieSettings

sessionApiServer
  :: CookieSettings
  -> JWTSettings
  -> AuthResult CookieSession
  -> ServerT WhoAmiApi (AppM)
sessionApiServer cookieSettings jwtSettings cookieSessionAuthResult =
  whoAmIHandler cookieSettings jwtSettings cookieSessionAuthResult

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
      getRequestCollectionById
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
  key <- generateKey
  let
    settings = setPort (naturalToInt $ port config) $ defaultSettings
    tlsOpts = tlsSettings "cert.pem" "key.pem"
  runTLS tlsOpts settings =<< mkApp config

mkApp :: Config -> IO Application
mkApp config = do
  let
    key = fromSecret $ BSU.fromString $ appKey config
    jwtSettings = defaultJWTSettings key
    cookieSettings =
      defaultCookieSettings { cookieIsSecure = Secure
                            , cookieSameSite = AnySite
                            , cookieXsrfSetting = Nothing
                            , sessionCookieName = "JWT"
                            }
    context = cookieSettings :. jwtSettings :. EmptyContext
    combinedApiProxy = Proxy :: Proxy (CombinedApi '[JWT])
    apiServer =
      session3ApiServer :<|>
      ( protectedApiServer :<|>
        ( loginApiServer cookieSettings jwtSettings :<|>
          sessionApiServer cookieSettings jwtSettings
        )
      ) :<|>
      testApiServer :<|>
      assetApiServer
    server =
      hoistServerWithContext combinedApiProxy (Proxy :: Proxy '[CookieSettings, JWTSettings]) (appMToHandler config) apiServer
  return $
    serveWithContext combinedApiProxy context server
