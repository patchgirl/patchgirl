{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Request where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Functor
import           Data.Maybe
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import           GHC.Generics
import           Servant

-- * DB

getByRequestId :: Int -> Connection -> IO (Maybe Request)
getByRequestId requestId connection = do
  listToMaybe <$> query connection rawQuery (Only requestId)
  where
    rawQuery = [sql|
              select id, text
              from request
              where id = ?
              |] :: Query

-- * MODEL

data Request
  = Request {
    requestId   :: Integer,
    requestText :: String
  }
  deriving (Eq, Show, Generic, FromRow)

instance ToJSON Request
instance FromJSON Request

-- * Handler

getRequests :: Handler [Request]
getRequests = return [exampleRequest]

getRequestById :: Int -> Handler Request
getRequestById requestId = do
  liftIO (getDBConnection >>= (getByRequestId requestId)) >>= \case
    Just request -> return request
    Nothing      -> throwError err404

-- * DB

exampleRequest :: Request
exampleRequest = Request 0 "example request"
