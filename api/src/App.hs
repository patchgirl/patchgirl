{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Network.Wai              hiding (Request)
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

import           RequestCollection
import           RequestNode
import AppHealth

-- * API

type CombinedApi =
  Api :<|> Public

type Api =
  RequestCollectionApi :<|>
  RequestFileApi :<|>
  HealthApi


type RequestCollectionApi =
  "requestCollection" :> (
    Capture "requestCollectionId" Int :> Get '[JSON] RequestCollection :<|>
    ReqBody '[JSON] [RequestNode] :> Post '[JSON] RequestCollection
  )

type RequestFileApi =
  "requestCollection" :> (
    Capture "requestCollectionId" Int :> "requestFile" :> ReqBody '[JSON] NewRequestFile :> Post '[JSON] CreatedRequestFile :<|>
    Capture "requestCollectionId" Int :> "requestFile" :> Capture "requestFileId" Int :> Put '[JSON] CreatedRequestFile
    )

type HealthApi =
  "health" :> Get '[JSON] AppHealth

type Public =
  "public" :> Raw

-- * Server

api :: Server Api
api =
  requestCollectionApi :<|>
  requestFileApi :<|>
  getAppHealth
  where
    requestCollectionApi =
      getRequestCollectionById :<|> postRequestCollection
    requestFileApi =
      createRequestFile :<|> updateRequestFile

public :: Server Public
public =
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
  return $ serve combinedApiProxy (api :<|> public)
