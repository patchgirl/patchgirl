{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RequestNode.Sql where

import           Control.Lens.Getter              ((^.))
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ

import           RequestNode.Model


selectRequestNodesFromRequestCollectionId :: Int -> PG.Connection -> IO [RequestNode]
selectRequestNodesFromRequestCollectionId requestCollectionId connection = do
    idToMRequestNodesFromPG <- PG.query connection selectRequestNodeSql (PG.Only requestCollectionId) :: IO[(Int, RequestNodeFromPG)]
    return $ map (fromPgRequestNodeToRequestNode . snd) idToMRequestNodesFromPG
  where
    selectRequestNodeSql =
      [sql|
          SELECT 1, UNNEST(root_request_nodes_as_json(?));
          |] :: PG.Query

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

updateRequestNodeDB :: Int -> UpdateRequestNode -> PG.Connection -> IO ()
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
