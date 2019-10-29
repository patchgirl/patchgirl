{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

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
import Http

-- * Model

data RequestCollection =
  RequestCollection Int [RequestNode]
  deriving (Eq, Show, Generic, ToJSON, FromJSON, FromRow)

data RequestNode
  = RequestFolder { id :: Int
                  , name :: String
                  , children :: [RequestNode]
                  }
  | RequestFile { id :: Int
                , name :: String
                , url :: String
                , method :: Method
                , headers :: [(String, String)]
                , body :: String
                }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)


instance FromField [RequestNode] where
  fromField field mdata = do
    value <- fromField field mdata :: Conversion Value
    let errorOrRequestNodes = (parseEither parseJSON) value :: Either String [RequestNode]
    either (returnError ConversionFailed field) return errorOrRequestNodes

-- * DB

selectRequestCollectionById :: Int -> Connection -> IO (Maybe RequestCollection)
selectRequestCollectionById requestCollectionId connection = do
  listToMaybe <$> query connection rawQuery (Only requestCollectionId)
  where
    rawQuery =
      [sql|
          WITH RECURSIVE request_node_from_parents AS
          (
            SELECT id, name, tag, '{}'::int[] AS parents, 0 AS level
            FROM request_node
            WHERE request_node_parent_id IS NULL
            AND request_collection_id = ?

            UNION ALL

            SELECT c.id, c.name, c.tag, parents || c.request_node_parent_id, level + 1
            FROM request_node_from_parents p
            JOIN request_node c
            ON c.request_node_parent_id = p.id
          ),
          request_node_from_children AS
          (
            SELECT c.request_node_parent_id,
            json_agg(
              jsonb_build_object('name', c.name) || jsonb_build_object('tag', c.tag)
            )::jsonb AS js
            FROM request_node_from_parents tree
            JOIN request_node c USING(id)
            WHERE NOT id = ANY(parents) AND level > 0
            GROUP BY c.request_node_parent_id

            UNION ALL

            SELECT
              c.request_node_parent_id,
              jsonb_build_object('name', c.name) ||
              jsonb_build_object('children', js) ||
              jsonb_build_object('tag', c.tag) AS js
            FROM request_node_from_children tree
            JOIN request_node c ON c.id = tree.request_node_parent_id
          )
          SELECT jsonb_pretty(jsonb_agg(js))
          FROM request_node_from_children
          WHERE request_node_parent_id IS NULL;

          SELECT id, tree
          FROM request_collection
          WHERE id = ?
          |] :: Query

-- * Handler

getRequestCollectionById :: Int -> Handler RequestCollection
getRequestCollectionById requestCollectionId = do
  liftIO (getDBConnection >>= (selectRequestCollectionById requestCollectionId)) >>= \case
    Just request -> return request
    Nothing      -> throwError err404
