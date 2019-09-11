{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Network.Wai              hiding (Request)
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

import           Request

-- * API

type Api =
  "request" :> Get '[JSON] [Request] :<|>
  "request" :> Capture "requestId" Int :> Get '[JSON] Request

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
  getRequests :<|>
  getRequestById
