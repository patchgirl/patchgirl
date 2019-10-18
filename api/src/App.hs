{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Network.Wai              hiding (Request)
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

import           RequestCollection
import AppHealth

-- * API

type Api =
  "requestCollection" :> Capture "requestCollectionId" Int :> Get '[JSON] RequestCollection :<|>
  "requestCollection" :> ReqBody '[JSON] [RequestNode] :> Post '[JSON] RequestCollection :<|>
  "health" :> Get '[JSON] AppHealth :<|>
  "public" :> Raw

api :: Proxy Api
api = Proxy

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
mkApp = return $ serve api server

server :: Server Api
server =
  getRequestCollectionById :<|>
  postRequestCollection :<|>
  getAppHealth :<|>
  serveDirectoryWebApp "../public"
