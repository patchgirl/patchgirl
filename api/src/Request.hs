{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

module Request where

import           Data.Aeson
import           GHC.Generics
import           Servant

-- * MODEL

data Request
  = Request {
    requestId   :: Integer,
    requestText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Request
instance FromJSON Request

-- * Handler

getRequests :: Handler [Request]
getRequests = return [exampleRequest]

getRequestById :: Integer -> Handler Request
getRequestById = \case
  0 -> return exampleRequest
  _ -> throwError err404

-- * DB

exampleRequest :: Request
exampleRequest = Request 0 "example request"
