{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RequestNode.Sql where

import           Control.Lens.Getter              ((^.))
import qualified Control.Monad                    as Monad
import           Data.UUID
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ

import           RequestNode.Model


-- * select request nodes from request collection id


selectRequestNodesFromRequestCollectionId :: Int -> PG.Connection -> IO [RequestNode]
selectRequestNodesFromRequestCollectionId requestCollectionId connection = do
    idToMRequestNodesFromPG <- PG.query connection selectRequestNodeSql (PG.Only requestCollectionId) :: IO[(Int, RequestNodeFromPG)]
    return $ map (fromPgRequestNodeToRequestNode . snd) idToMRequestNodesFromPG
  where
    selectRequestNodeSql =
      [sql|
          SELECT 1, UNNEST(root_request_nodes_as_json(?));
          |] :: PG.Query


-- * insert request file


insertRequestFile :: NewRequestFile -> PG.Connection -> IO ()
insertRequestFile NewRequestFile { _newRequestFileId
                                 , _newRequestFileParentNodeId
                                 } connection =
  Monad.void $ PG.execute connection rawQuery (_newRequestFileId, _newRequestFileParentNodeId)
  where
    rawQuery =
      [sql|
          INSERT INTO request_node (
            id,
            request_node_parent_id,
            tag,
            name,
            http_url,
            http_method,
            http_headers,
            http_body
          )
          VALUES (?, ?, 'RequestFile', 'new request', '', 'Get', '{}', '')
          |]


-- * insert request folder


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

updateRequestNodeDB :: UUID -> UpdateRequestNode -> PG.Connection -> IO ()
updateRequestNodeDB requestNodeId updateRequestNode connection = do
  -- todo search func with : m a -> m b
  let newName = updateRequestNode ^. updateRequestNodeName
  _ <- PG.execute connection updateQuery (newName, requestNodeId)
  print updateRequestNode
  return ()
  where
    updateQuery =
      [sql|
          UPDATE request_node
          SET name = ?
          WHERE id = ?
          |]
