{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module RequestCollection where

import           Control.Monad.Except               (MonadError)
import           Control.Monad.IO.Class             (MonadIO)
import           Control.Monad.IO.Class
import           Control.Monad.Reader               (MonadReader)
import           Data.Aeson
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow (FromRow (..))
import           Database.PostgreSQL.Simple.SqlQQ
import           DB
import           GHC.Generics
import           PatchGirl
import           RequestNode.Model
import           Servant

-- * Model

data RequestCollection =
  RequestCollection Int [RequestNode]
  deriving (Eq, Show, Generic, ToJSON, FromJSON, FromRow)

-- * DB

selectRequestCollectionById :: Int -> Connection -> IO (Maybe RequestCollection)
selectRequestCollectionById requestCollectionId connection = do
  query connection collectionExistsSql (Only requestCollectionId) >>= \case
    [Only True] -> do
      [(_, requestNodesFromPG)] <- query connection selectRequestCollectionSql (requestCollectionId, requestCollectionId) :: IO[(Int, [RequestNodeFromPG])]
      return . Just $
        RequestCollection requestCollectionId (fromPgRequestNodeToRequestNode <$> requestNodesFromPG)
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
              request_node_as_js(c) || CASE WHEN jsonb_typeof(js) = 'array' THEN
                jsonb_build_object('children', js)
              ELSE
                jsonb_build_object('children', jsonb_build_array(js))
              END AS js
            FROM request_node_from_leaves tree
            JOIN request_node c ON c.id = tree.request_node_parent_id
          )

          SELECT
            ? AS request_collection_id,
            jsonb_agg(js) AS request_nodes
          FROM request_node_from_leaves
          WHERE request_node_parent_id IS NULL;
          |] :: Query

-- * Handler

getRequestCollectionById
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => Int
  -> m RequestCollection
getRequestCollectionById requestCollectionId = do
  liftIO (getDBConnection >>= (selectRequestCollectionById requestCollectionId)) >>= \case
    Just request -> return request
    Nothing      -> throwError err404
