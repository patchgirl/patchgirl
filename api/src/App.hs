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
import RequestNode.Model
import AppHealth


-- * API

type CombinedApi =
  RestApi :<|> AssetApi

type RestApi =
  RequestCollectionApi :<|>
  RequestNodeApi :<|>
  RequestFileApi :<|>
  HealthApi

type RequestCollectionApi =
  "requestCollection" :> (
    -- getRequestCollectionById
    Capture "requestCollectionId" Int :> Get '[JSON] RequestCollection
  )

type RequestNodeApi = Flat (
  "requestCollection" :> Capture "requestCollectionId" Int :> "requestNode" :> Capture "requestNodeId" Int :> (
    "_rename" :> ReqBody '[JSON] String :> Put '[JSON] NoContent -- :<|>
--    "_delete" :> Delete '[JSON] NoContent
    )
  )

type RequestFileApi = Flat (
  "requestCollection" :> Capture "requestCollectionId" Int :> "requestFile" :> (
    -- createRequestFile
    ReqBody '[JSON] NewRequestFile :> Post '[JSON] Int :<|>
    -- updateRequestFile
    Capture "requestFileId" Int :> Put '[JSON] Int
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
  getAppHealth
  where
    requestCollectionApi =
      getRequestCollectionById
    requestNodeApi =
      renameNodeRequest -- :<|> undefined
    requestFileApi =
      createRequestFile :<|> updateRequestFile

assetApiServer :: Server AssetApi
assetApiServer =
  serveDirectoryWebApp "../public"

-- * Proxy

requestCollectionApiProxy :: Proxy RequestCollectionApi
requestCollectionApiProxy = Proxy

requestFileApiProxy :: Proxy RequestFileApi
requestFileApiProxy = Proxy

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
  return $ serve combinedApiProxy (restApiServer :<|> assetApiServer)
