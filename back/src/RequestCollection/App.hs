{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module RequestCollection.App where

import           Control.Monad.Except               (MonadError)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
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
import           Session.Model


-- * Model


data RequestCollection =
  RequestCollection Int [RequestNode]
  deriving (Eq, Show, Generic, ToJSON, FromJSON, FromRow)


-- * DB


selectRequestCollectionAvailable :: Int -> Int -> Connection -> IO Bool
selectRequestCollectionAvailable accountId requestCollectionId connection =
  query connection collectionExistsSql (requestCollectionId, accountId) >>= \case
    [Only True] -> return True
    _ -> return False
  where
    collectionExistsSql =
      [sql|
          SELECT EXISTS (
            SELECT 1
            FROM request_collection
            WHERE id = ?
            AND account_id = ?
          );
          |]

selectRequestCollectionById :: Int -> Connection -> IO (Maybe RequestCollection)
selectRequestCollectionById requestCollectionId connection = do
    [(_, requestNodesFromPG)] <- query connection selectRequestCollectionSql (requestCollectionId, requestCollectionId) :: IO[(Int, [RequestNodeFromPG])]
    return . Just $
      RequestCollection requestCollectionId (fromPgRequestNodeToRequestNode <$> requestNodesFromPG)
  where
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


getRequestCollectionHandler
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => Int
  -> Int
  -> m RequestCollection
getRequestCollectionHandler accountId requestCollectionId = do
  connection <- getDBConnection
  liftIO (selectRequestCollectionAvailable accountId requestCollectionId connection) >>= \case
    False -> throwError err404
    True ->
      liftIO (selectRequestCollectionById requestCollectionId connection) >>= \case
        Just request -> return request
        Nothing      -> throwError err404

getRequestCollectionHandler2
  :: ( MonadReader Config m
     , MonadIO m
     , MonadError ServerError m
     )
  => Int
  -> Int
  -> m RequestCollection
getRequestCollectionHandler2 accountId requestCollectionId = do
  connection <- getDBConnection
  liftIO (selectRequestCollectionAvailable accountId requestCollectionId connection) >>= \case
    False -> throwError err404
    True ->
      liftIO (selectRequestCollectionById requestCollectionId connection) >>= \case
        Just request -> return request
        Nothing      -> throwError err404


{-
data RequestNodeFromPG2 =
  RequestNodeFromPG2 { requestNodeFromPG2Id :: Int
                     , requestNodeFromPG2ParentId :: Int
                     , requestNodeFromPG2Tag :: String
                     , requestNodeFromPG2Name :: String
                     , requestNodeFromPG2HttpUrl :: String
                     , requestNodeFromPG2HttpMethod :: String
                     , requestNodeFromPG2HttpHeaders :: String
                     , requestNodeFromPG2HttpBody :: String
                     }


selectRequestCollection :: Int -> Connection -> IO (Maybe RequestCollection)
selectRequestCollection accountId connection = do
    [(requestCollectionId, rootRequestNodeId) :: (Int, Int)] <- query connection selectRootRequestNodeId (Only accountId)
    requestNodesFromPG :: [RequestNodeFromPG2] <- query connection selectRequestCollectionSql (requestCollectionId, requestCollectionId)
    return . Just $
      RequestCollection2 requestCollectionId rootRequestNodeId (buildRequestNodeFromPG requestNodesFromPG)
  where
    selectRootRequestNodeId =
      [sql|
          SELECT id, root_request_node_id
          FROM request_collection2
          WHERE account_id = ?
          |]

    selectRequestCollectionSql =
      [sql|
          WITH RECURSIVE request_node_with_its_parent(node_id, parents) AS (
            SELECT root_request_node_id, NULL :: int
            FROM request_collection2 n
            WHERE id = ?

            UNION ALL

            SELECT c.id, c.request_node_parent_id
            FROM request_node_with_its_parent p
            JOIN request_node2 c
            ON c.request_node_parent_id = p.node_id
          ) SELECT id, request_node_parent_id, tag, name, http_url, http_method, http_headers, http_body
            FROM request_node_with_its_parent
            INNER JOIN request_node2 r ON r.id = node_id
            WHERE request_node_parent_id IS NOT NULL
          |] :: Query
-}
