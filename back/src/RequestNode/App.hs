{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RequestNode.App where

import qualified Control.Monad.Except             as Except (MonadError)
import qualified Control.Monad.IO.Class           as IO
import qualified Control.Monad.Reader             as Reader
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ
import qualified Servant
import qualified Servant.API.ContentTypes         as API

import           DB
import           PatchGirl
import           RequestCollection.Sql
import           RequestNode.Model

-- * update request node


-- ** handler


updateRequestNodeHandler
  :: ( Reader.MonadReader Config m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Int
  -> Int
  -> Int
  -> UpdateRequestNode
  -> m API.NoContent
updateRequestNodeHandler accountId requestCollectionId requestNodeId updateRequestNode = do
  connection <- getDBConnection
  requestNodeIds <- IO.liftIO $ requestNodeIdsFromCollectionId requestCollectionId connection
  requestCollectionAvailable <- IO.liftIO $ selectRequestCollectionAvailable accountId requestCollectionId connection
  case requestNodeId `elem` requestNodeIds && requestCollectionAvailable of
    False -> Servant.throwError Servant.err404
    True ->
      IO.liftIO $
        updateRequestNodeDB requestNodeId updateRequestNode connection >> return API.NoContent


-- ** db


requestNodeIdsFromCollectionId :: Int -> PG.Connection -> IO [Int]
requestNodeIdsFromCollectionId requestCollectionId connection = do
  ids :: [PG.Only Int] <- PG.query connection requestNodeIdsFromCollectionIdQuery (PG.Only requestCollectionId)
  return $ map (\(PG.Only id) -> id) ids
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

updateRequestNodeDB :: Int -> UpdateRequestNode -> PG.Connection -> IO ()
updateRequestNodeDB requestNodeId updateRequestNode connection = do
  -- todo search func with : m a -> m b
  _ <- PG.execute connection updateQuery (updateRequestNode, requestNodeId)
  return ()
  where
    updateQuery =
      [sql|
          UPDATE request_node
          SET name = ?
          WHERE id = ?
          |]


-- *  create request file


-- ** handler


createRequestFileHandler
  :: ( Reader.MonadReader Config m
     , IO.MonadIO m
     , Except.MonadError Servant.ServerError m
     )
  => Int
  -> Int
  -> NewRequestFile
  -> m Int
createRequestFileHandler _ _ newRequestFile = do
  connection <- getDBConnection
  IO.liftIO $ insertRequestFile newRequestFile connection


-- ** db


insertRequestFile :: NewRequestFile -> PG.Connection -> IO Int
insertRequestFile newRequestFile connection = do
  [PG.Only id] <- PG.query connection rawQuery newRequestFile
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO request_node (
            name,
            request_node_parent_id,
            http_method
            tag,
          )
          VALUES (?, ?, ?, ?, ?)
          RETURNING id
          |]


-- * create request folder


insertRequestFolder :: NewRequestFolder -> PG.Connection -> IO Int
insertRequestFolder newRequestFolder connection = do
  [PG.Only id] <- PG.query connection rawQuery newRequestFolder
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
