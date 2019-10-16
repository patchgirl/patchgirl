{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances       #-}

module RequestCollection where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import Data.Aeson.Types
import           Data.Functor
import           Data.Maybe
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.TypeInfo as TI
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import           GHC.Generics
import           Servant
import           Data.Aeson (encode)

-- * Model

data RequestCollection =
  RequestCollection Int [RequestNode]
  deriving (Eq, Show, Generic)

instance ToJSON RequestCollection
instance FromJSON RequestCollection
instance FromRow RequestCollection

data RequestNode
  = RequestFolder { name :: String
                  , children :: [RequestNode]
                  }
  | Request2 { name :: String
             , url :: String
             }
  deriving (Eq, Show, Generic)

instance ToJSON RequestNode
instance FromJSON RequestNode

instance ToField [RequestNode] where
  toField = toField . encode

instance FromField [RequestNode] where
  fromField field mdata = do
    value <- fromField field mdata :: Conversion Value
    let errorOrRequestNodes = (parseEither parseJSON) value :: Either String [RequestNode]
    either (returnError ConversionFailed field) return errorOrRequestNodes

data Request
  = Request {
    requestId   :: Int,
    requestText :: String
    }
  deriving (Eq, Show, Generic, FromRow)

instance ToJSON Request
instance FromJSON Request

-- * DB

insertRequestNodes :: Connection -> [RequestNode] -> IO RequestCollection
insertRequestNodes connection requestNodes = do
  [requestCollection] <- query connection rawQuery $ Only requestNodes
  return requestCollection
  where
    rawQuery =
      [sql|
          INSERT INTO request_collection (tree)
          VALUES (?)
          RETURNING id, tree
          |]

selectRequestCollectionById :: Int -> Connection -> IO (Maybe RequestCollection)
selectRequestCollectionById requestCollectionId connection = do
  listToMaybe <$> query connection rawQuery (Only requestCollectionId)
  where
    rawQuery =
      [sql|
          SELECT id, tree
          FROM request_collection
          WHERE id = ?
          |] :: Query


selectRequestById :: Int -> Connection -> IO (Maybe Request)
selectRequestById requestId connection = do
  listToMaybe <$> query connection rawQuery (Only requestId)
  where
    rawQuery =
      [sql|
          SELECT id, text
          FROM request
          WHERE id = ?
          |] :: Query

selectRequests :: Connection -> IO [Request]
selectRequests connection = do
  query_ connection rawQuery
  where
    rawQuery =
      [sql|
          SELECT id, text
          FROM request
          |] :: Query

-- * Handler

getRequestCollectionById :: Int -> Handler RequestCollection
getRequestCollectionById requestCollectionId = do
  liftIO (getDBConnection >>= (selectRequestCollectionById requestCollectionId)) >>= \case
    Just request -> return request
    Nothing      -> throwError err404

getRequests :: Handler [Request]
getRequests = do
  liftIO (getDBConnection >>= selectRequests)

getRequestById :: Int -> Handler Request
getRequestById requestId = do
  liftIO (getDBConnection >>= (selectRequestById requestId)) >>= \case
    Just request -> return request
    Nothing      -> throwError err404
