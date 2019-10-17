{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances       #-}

module RequestCollection where

import           Control.Monad.IO.Class
import           Data.Aeson
import Data.Aeson.Types (parseEither)
import           Data.Maybe
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
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
  | RequestFile { name :: String
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

insertRequestNodes :: [RequestNode] -> Connection -> IO RequestCollection
insertRequestNodes requestNodes connection = do
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

-- * Handler

postRequestCollection :: [RequestNode] -> Handler RequestCollection
postRequestCollection requestNodes = do
  -- insertRequestNodes :: Connection -> [RequestNode] -> IO RequestCollection
  liftIO (getDBConnection >>= (insertRequestNodes requestNodes)) >>= return

getRequestCollectionById :: Int -> Handler RequestCollection
getRequestCollectionById requestCollectionId = do
  liftIO (getDBConnection >>= (selectRequestCollectionById requestCollectionId)) >>= \case
    Just request -> return request
    Nothing      -> throwError err404
