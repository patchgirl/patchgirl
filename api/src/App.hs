{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Network.Wai              hiding (Request)
import           Network.Wai.Handler.Warp
import           Servant
import Servant.API.Flatten (Flat)
import Servant.API.ContentTypes (NoContent)
import           System.IO
import           RequestCollection
import           RequestNode.App
import           Environment.App
import RequestNode.Model
import AppHealth
import Test

-- * API

type CombinedApi =
  RestApi :<|> TestApi :<|> AssetApi

type RestApi =
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
      ReqBody '[JSON] [UpsertKeyValue] :> Put '[JSON] [KeyValue] :<|> -- upsertKeyValues
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

restApiServer :: Server RestApi
restApiServer =
  requestCollectionApi :<|>
  requestNodeApi :<|>
  requestFileApi :<|>
  environmentApi :<|>
  getAppHealth
  where
    requestCollectionApi =
      getRequestCollectionById
    requestNodeApi =
      updateRequestNodeHandler -- renameNodeRequest -- :<|> undefined
    requestFileApi =
      createRequestFile -- :<|> updateRequestFile
    environmentApi =
      createEnvironmentHandler :<|>
      getEnvironmentsHandler :<|>
      updateEnvironmentHandler :<|>
      deleteEnvironmentHandler :<|>
      upsertKeyValuesHandler :<|>
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

combinedApiProxy :: Proxy CombinedApi
combinedApiProxy = Proxy

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
mkApp =
  return $ serve combinedApiProxy (restApiServer :<|> testApiServer :<|> assetApiServer)
