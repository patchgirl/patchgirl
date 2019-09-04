{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Request where

import           Data.Aeson
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Servant

foo :: IO ()
foo = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "test"
  }
  putStrLn "2 + 2"
  mapM_ print =<< ( query_ conn "select 2 + 2" :: IO [Only Int] )

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
