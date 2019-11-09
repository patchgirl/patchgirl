{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module RequestNode.App where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics
import           Servant (Handler, throwError, err404)
import Servant.API.ContentTypes (NoContent(..))
import           Control.Monad.IO.Class (liftIO)
import           DB
import Http
import           Data.Aeson (FromJSON, ToJSON)
import RequestNode.Model
import           Database.PostgreSQL.Simple.FromField

-- * request node

requestNodeIdsFromCollectionId :: Int -> Connection -> IO [Int]
requestNodeIdsFromCollectionId requestCollectionId connection = do
  oIds <- (query connection requestNodeIdsFromCollectionIdQuery $ Only requestCollectionId) :: IO [Only Int]
  return $
    map (\oId ->
           let Only id = oId
           in id
        ) oIds
  where
    requestNodeIdsFromCollectionIdQuery =
      [sql|
          WITH RECURSIVE request_node_with_its_parent AS (
            SELECT id, '{}'::int[] AS parents
            FROM request_node n
            INNER JOIN request_collection_to_request_node cn ON(cn.request_node_id = n.id)
            WHERE request_node_parent_id IS NULL
            AND cn.request_collection_id = ?

            UNION ALL

            SELECT c.id, parents || c.request_node_parent_id
            FROM request_node_with_its_parent p
            JOIN request_node c
            ON c.request_node_parent_id = p.id
          )
          SELECT id
          FROM request_node_with_its_parent;
          |]

updateRequestNodeDB :: Int -> UpdateRequestNode -> Connection -> IO ()
updateRequestNodeDB requestNodeId updateRequestNode connection = do
  execute connection updateQuery $ (updateRequestNode, requestNodeId)
  return ()
  where
    updateQuery =
      [sql|
          UPDATE request_node
          SET name = ?
          WHERE id = ?
          |]

updateRequestNodeHandler :: Int -> Int -> UpdateRequestNode -> Handler NoContent
updateRequestNodeHandler requestCollectionId requestNodeId updateRequestNode = do
  connection <- liftIO getDBConnection
  requestNodeIds <- liftIO $ requestNodeIdsFromCollectionId requestCollectionId connection
  case requestNodeId `elem` requestNodeIds of
    True -> (liftIO $ updateRequestNodeDB requestNodeId updateRequestNode connection) >> return NoContent
    False -> throwError err404

-- * file

insertRequestFile :: NewRequestFile -> Connection -> IO Int
insertRequestFile newRequestFile connection = do
  [Only id] <- query connection rawQuery $ newRequestFile
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO request_node (
            request_collection_id,
            request_node_parent_id,
            tag,
            name,
            http_method
          )
          VALUES (?, ?, ?, ?, ?)
          RETURNING id
          |]

createRequestFile :: Int -> NewRequestFile -> Handler Int
createRequestFile requestCollectionId newRequestFile = do
  liftIO (getDBConnection >>= (insertRequestFile newRequestFile)) >>= return

updateRequestFile :: Int -> Int -> Handler Int
updateRequestFile requestCollectionId requestFileId =
  undefined

-- * folder

insertRequestFolder :: NewRequestFolder -> Connection -> IO Int
insertRequestFolder newRequestFolder connection = do
  [Only id] <- query connection rawQuery $ newRequestFolder
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO request_node (
            request_collection_id,
            request_node_parent_id,
            tag,
            name
          )
          VALUES (?, ?, ?, ?)
          RETURNING id
          |]
