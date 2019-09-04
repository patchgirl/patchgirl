{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai              hiding (Request)
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

-- * API

type RequestApi =
  "request" :> Get '[JSON] [Request] :<|>
  "request" :> Capture "requestId" Integer :> Get '[JSON] Request

requestApi :: Proxy RequestApi
requestApi = Proxy

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
mkApp = return $ serve requestApi server

server :: Server RequestApi
server =
  getRequests :<|>
  getRequestById

getRequests :: Handler [Request]
getRequests = return [exampleRequest]

getRequestById :: Integer -> Handler Request
getRequestById = \case
  0 -> return exampleRequest
  _ -> throwError err404

exampleRequest :: Request
exampleRequest = Request 0 "example request"

-- * MODEL

data Request
  = Request {
    requestId   :: Integer,
    requestText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Request
instance FromJSON Request
