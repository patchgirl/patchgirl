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

-- * MODEL

data Request
  = Request {
    requestId   :: Int,
    requestText :: String
  }
  deriving (Eq, Show, Generic, FromRow)

instance ToJSON Request
instance FromJSON Request

-- * DB

selectRequestById :: Int -> Connection -> IO (Maybe Request)
selectRequestById requestId connection = do
  listToMaybe <$> query connection rawQuery (Only requestId)
  where
    rawQuery = [sql|
                   SELECT id, text
                   FROM request
                   WHERE id = ?
                   |] :: Query

selectRequests :: Connection -> IO [Request]
selectRequests connection = do
  query_ connection rawQuery
  where
    rawQuery = [sql|
                   SELECT id, text
                   FROM request
                   |] :: Query

-- * Handler

getRequests :: Handler [Request]
getRequests = do
  liftIO (getDBConnection >>= selectRequests)

getRequestById :: Int -> Handler Request
getRequestById requestId = do
  liftIO (getDBConnection >>= (selectRequestById requestId)) >>= \case
    Just request -> return request
    Nothing      -> throwError err404

-- * DB

exampleRequest :: Request
exampleRequest = Request 0 "example request"
