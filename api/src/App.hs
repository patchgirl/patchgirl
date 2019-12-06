{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module App where

import AppHealth
import Environment.App
import Network.Wai hiding (Request)
import Network.Wai.Handler.Warp
import RequestNode.App
import RequestCollection
import RequestNode.Model
import Servant
import Servant.API.ContentTypes (NoContent)
import Servant.API.Flatten (Flat)
import Servant.Auth.Server (Auth, AuthResult(..), generateKey, defaultJWTSettings, defaultCookieSettings, JWT, throwAll, SetCookie, CookieSettings, JWTSettings)
import System.IO
import Test
import Session.App
import Session.Model

-- * API


type CombinedApi auths =
  (RestApi auths) :<|>
  TestApi :<|>
  AssetApi

type RestApi auths =
  (Auth auths Session :> ProtectedApi) :<|>
  (Auth auths Session :> SessionApi)

type SessionApi =
  Flat (
    "session" :> (
        "login" :>
        ReqBody '[JSON] Login :>
        PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                        , Header "Set-Cookie" SetCookie
                                        ] NoContent) :<|>
        "whoami" :>
        Get '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                              , Header "Set-Cookie" SetCookie
                              ] Session)


    )
  )

type ProtectedApi =
  RequestCollectionApi :<|>
  RequestNodeApi :<|>
  RequestFileApi :<|>
  EnvironmentApi :<|>
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


-- * Server

sessionApiServer
  :: CookieSettings
  -> JWTSettings
  -> AuthResult Session
  -> Server SessionApi
sessionApiServer cookieSettings jwtSettings sessionAuthResult =
  createSessionHandler cookieSettings jwtSettings :<|>
  whoAmIHandler cookieSettings jwtSettings sessionAuthResult

protectedApiServer :: AuthResult Session -> Server ProtectedApi
protectedApiServer = \case

  Authenticated _ ->
    requestCollectionApi :<|>
    requestNodeApi :<|>
    requestFileApi :<|>
    environmentApi :<|>
    getAppHealth

  _ -> throwAll err401

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

testApiServer :: Server TestApi
testApiServer =
  testApi
  where
    testApi =
      deleteNoContentHandler :<|>
      getNotFoundHandler :<|>
      getInternalServerErrorHandler

assetApiServer :: Server AssetApi
assetApiServer =
  serveDirectoryWebApp "../public"


-- * Proxy


combinedApiProxy :: Proxy (CombinedApi '[JWT])
combinedApiProxy = Proxy

restApiProxy :: Proxy (RestApi '[JWT])
restApiProxy = Proxy

requestCollectionApiProxy :: Proxy RequestCollectionApi
requestCollectionApiProxy = Proxy

requestFileApiProxy :: Proxy RequestFileApi
requestFileApiProxy = Proxy

testApiProxy :: Proxy TestApi
testApiProxy = Proxy

environmentApiProxy :: Proxy EnvironmentApi
environmentApiProxy = Proxy

keyValueApiProxy :: Proxy KeyValueApi
keyValueApiProxy = Proxy

healthApiProxy :: Proxy HealthApi
healthApiProxy = Proxy

sessionApiProxy :: Proxy SessionApi
sessionApiProxy = Proxy


-- * APP


run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = do
  myKey <- generateKey
  let
    jwtSettings = defaultJWTSettings myKey
    cookieSettings = defaultCookieSettings
    context = cookieSettings :. jwtSettings :. EmptyContext
    apiServer =
      (protectedApiServer :<|> (sessionApiServer cookieSettings jwtSettings)) :<|>
      testApiServer :<|>
      assetApiServer
  return $
    serveWithContext combinedApiProxy context apiServer
