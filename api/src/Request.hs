{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Request where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Text
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import           GHC.Generics
import           Servant
import           Text.RawString.QQ

-- * DB

getOne :: Connection -> IO String
getOne connection = do
  [(age, name)] <- (query_ connection query :: IO [(Int, String)])
  return name
  where
    query = [sql|
                select id, text
                from request
                limit 1
                |]

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
  0 -> do
    connection <- liftIO getDBConnection
    text <- liftIO (getOne connection)
    liftIO $ print text
    return exampleRequest
  _ -> throwError err404

-- * DB

exampleRequest :: Request
exampleRequest = Request 0 "example request"
