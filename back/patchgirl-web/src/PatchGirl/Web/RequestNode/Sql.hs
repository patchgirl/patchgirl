
module PatchGirl.Web.RequestNode.Sql where

import           Control.Lens.Getter              ((^.))
import qualified Control.Monad                    as Monad
import           Data.UUID
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ

import           PatchGirl.Web.RequestNode.Model


-- * select request nodes from request collection id


selectRequestNodesFromRequestCollectionId :: Int -> PG.Connection -> IO [RequestNode]
selectRequestNodesFromRequestCollectionId requestCollectionId connection = do
    res :: [PG.Only RequestNodeFromPG] <- PG.query connection selectRequestNodeSql (PG.Only requestCollectionId)
    return $ map (fromPgRequestNodeToRequestNode . (\(PG.Only r) -> r)) res
  where
    selectRequestNodeSql =
      [sql|
          SELECT UNNEST(root_request_nodes_as_json(?));
          |] :: PG.Query


-- * update request node (rename)


updateRequestNodeDB :: UUID -> UpdateRequestNode -> PG.Connection -> IO ()
updateRequestNodeDB requestNodeId updateRequestNode connection = do
  -- todo search func with : m a -> m b
  let newName = updateRequestNode ^. updateRequestNodeName
  _ <- PG.execute connection updateQuery (newName, requestNodeId)
  return ()
  where
    updateQuery =
      [sql|
          UPDATE request_node
          SET name = ?
          WHERE id = ?
          |]


-- * delete request node


deleteRequestNodeDB :: UUID -> PG.Connection -> IO ()
deleteRequestNodeDB requestNodeId connection =
  Monad.void $ PG.execute connection updateQuery (PG.Only requestNodeId)
  where
    updateQuery =
      [sql|
          DELETE FROM request_node
          WHERE id = ?
          |]


-- * insert root request file


insertRootRequestFile :: NewRootRequestFile -> Int -> PG.Connection -> IO ()
insertRootRequestFile NewRootRequestFile {..} requestCollectionId connection =
  Monad.void $
    PG.execute connection rawQuery ( _newRootRequestFileId
                                   , _newRootRequestFileName
                                   , requestCollectionId
                                   , _newRootRequestFileId
                                   )
  where
    rawQuery =
      [sql|
          WITH insert_root_request_node AS (
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
            VALUES (?, NULL, 'RequestFile', ?, '', 'Get', '{}', '')
          ) INSERT INTO request_collection_to_request_node (
              request_collection_id,
              request_node_id
            )
            VALUES (?, ?)
          |]


-- * insert request file


insertRequestFile :: NewRequestFile -> PG.Connection -> IO ()
insertRequestFile NewRequestFile {..} connection =
  Monad.void $ PG.execute connection rawQuery (_newRequestFileId, _newRequestFileParentNodeId, _newRequestFileName)
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
          VALUES (?, ?, 'RequestFile', ?, '', 'Get', '{}', '')
          |]


-- * insert root request folder


insertRootRequestFolder :: NewRootRequestFolder -> Int -> PG.Connection -> IO ()
insertRootRequestFolder NewRootRequestFolder {..} requestCollectionId connection =
  Monad.void $
    PG.execute connection rawQuery ( _newRootRequestFolderId
                                   , _newRootRequestFolderName
                                   , requestCollectionId
                                   , _newRootRequestFolderId
                                   )
  where
    rawQuery =
      [sql|
          WITH insert_root_request_node AS (
            INSERT INTO request_node (
              id,
              request_node_parent_id,
              tag,
              name
            )
            VALUES (?, NULL, 'RequestFolder', ?)
          ) INSERT INTO request_collection_to_request_node (
              request_collection_id,
              request_node_id
            )
            VALUES (?, ?)
          |]


-- * insert request folder


insertRequestFolder :: NewRequestFolder -> PG.Connection -> IO ()
insertRequestFolder NewRequestFolder {..} connection =
  Monad.void $ PG.execute connection rawQuery (_newRequestFolderId, _newRequestFolderParentNodeId, _newRequestFolderName)
  where
    rawQuery =
      [sql|
          INSERT INTO request_node (
            id,
            request_node_parent_id,
            tag,
            name
          )
          VALUES (?, ?, 'RequestFolder', ?)
          |]


-- * update request file


updateRequestFileDB :: UUID -> UpdateRequestFile -> PG.Connection -> IO ()
updateRequestFileDB requestNodeId UpdateRequestFile{..} connection = do
  _ <- PG.execute connection updateQuery ( _updateRequestFileName
                                         , _updateRequestFileHttpUrl
                                         , _updateRequestFileHttpMethod
                                         , _updateRequestFileHttpHeaders
                                         , _updateRequestFileHttpBody
                                         , requestNodeId
                                         )
  return ()
  where
    updateQuery =
      [sql|
          UPDATE request_node
          SET
            name = ?,
            http_url = ?,
            http_method = ?,
            http_headers = ?,
            http_body = ?
          WHERE id = ?
          |]
