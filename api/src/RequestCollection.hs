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
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow (FromRow(..))
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import           GHC.Generics
import           Servant
import RequestNode.Model

-- * Model

data RequestCollection =
  RequestCollection Int [RequestNode]
  deriving (Eq, Show, Generic, ToJSON, FromJSON, FromRow)

-- * DB

selectRequestCollectionById :: Int -> Connection -> IO (Maybe RequestCollection)
selectRequestCollectionById requestCollectionId connection = do
  query connection collectionExistsSql (Only requestCollectionId) >>= \case
    [Only True] -> do
      [requestCollection] <- query connection selectRequestCollectionSql (requestCollectionId, requestCollectionId)
      return $ Just requestCollection
    _ ->
      return Nothing
  where
    collectionExistsSql =
      [sql|
          SELECT EXISTS (
            SELECT 1
            FROM request_collection_to_request_node
            WHERE request_collection_id = ?
          );
          |]
    selectRequestCollectionSql =
      [sql|
          WITH RECURSIVE request_node_with_its_parent AS (
            SELECT id, '{}'::int[] AS parents, 0 AS level
            FROM request_node n
            INNER JOIN request_collection_to_request_node cn ON(cn.request_node_id = n.id)
            WHERE request_node_parent_id IS NULL
            AND cn.request_collection_id = ?
            UNION ALL
            SELECT c.id, parents || c.request_node_parent_id, level + 1
            FROM request_node_with_its_parent p
            JOIN request_node c
            ON c.request_node_parent_id = p.id

          ), parent_ids AS (
            SELECT DISTINCT UNNEST(parents) AS id
            FROM request_node_with_its_parent

          ), leave_ids AS (
            SELECT r.id, level
            FROM request_node_with_its_parent r
            LEFT JOIN parent_ids p ON r.id = p.id
            WHERE p.id IS NULL

          ), request_node_from_leaves AS (
            (
              -- root leaves
              SELECT c.request_node_parent_id,
              request_node_as_js(c) AS js
              FROM leave_ids l
              JOIN request_node c USING(id)
              WHERE level = 0

              UNION ALL

              -- leaves
              SELECT c.request_node_parent_id,
              json_agg(request_node_as_js(c))::jsonb AS js
              FROM leave_ids l
              JOIN request_node c USING(id)
              WHERE level > 0
              GROUP BY request_node_parent_id
            )
            UNION ALL
            SELECT
              c.request_node_parent_id,
              request_node_as_js(c) || jsonb_build_object('children', js) AS js
            FROM request_node_from_leaves tree
            JOIN request_node c ON c.id = tree.request_node_parent_id
          )

          SELECT ?, jsonb_pretty(jsonb_agg(js))
          FROM request_node_from_leaves
          WHERE request_node_parent_id IS NULL;
          |] :: Query

-- * Handler

getRequestCollectionById :: Int -> Handler RequestCollection
getRequestCollectionById requestCollectionId = do
  liftIO (getDBConnection >>= (selectRequestCollectionById requestCollectionId)) >>= \case
    Just request -> return request
    Nothing      -> throwError err404
