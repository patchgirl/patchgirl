module PatchGirl.Web.PgNode.Sql where

import           Control.Lens.Getter              ((^.))
import qualified Control.Monad                    as Monad
import           Data.UUID                        (UUID)
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ

import           PatchGirl.Web.PgNode.Model


-- * select pg nodes from pg collection id


selectPgNodesFromPgCollectionId :: UUID -> PG.Connection -> IO [PgNode]
selectPgNodesFromPgCollectionId pgCollectionId connection = do
    res :: [PG.Only PgNodeFromPG] <- PG.query connection query (PG.Only pgCollectionId)
    return $ map (fromPgPgNodeToPgNode . (\(PG.Only r) -> r)) res
  where
    query =
      [sql|
          SELECT UNNEST(root_pg_nodes_as_json(?));
          |] :: PG.Query


-- * update pg node (rename)


updatePgNodeDB :: UUID -> UpdatePgNode -> PG.Connection -> IO ()
updatePgNodeDB pgNodeId updatePgNode connection = do
  -- todo search func with : m a -> m b
  let newName = updatePgNode ^. updatePgNodeName
  _ <- PG.execute connection updateQuery (newName, pgNodeId)
  return ()
  where
    updateQuery =
      [sql|
          UPDATE pg_node
          SET name = ?
          WHERE id = ?
          |]


-- * delete pg node


deletePgNodeDB :: UUID -> PG.Connection -> IO ()
deletePgNodeDB pgNodeId connection =
  Monad.void $ PG.execute connection updateQuery (PG.Only pgNodeId)
  where
    updateQuery =
      [sql|
          DELETE FROM pg_node
          WHERE id = ?
          |]


-- * insert root pg file


insertRootPgFile :: NewRootPgFile -> UUID -> PG.Connection -> IO ()
insertRootPgFile NewRootPgFile {..} pgCollectionId connection =
  Monad.void $
    PG.execute connection rawQuery ( _newRootPgFileId
                                   , _newRootPgFileName
                                   , _newRootPgFileSql
                                   , _newRootPgFileHost
                                   , _newRootPgFilePassword
                                   , _newRootPgFilePort
                                   , _newRootPgFileUser
                                   , _newRootPgFileDbName
                                   , pgCollectionId
                                   , _newRootPgFileId
                                   )
  where
    rawQuery =
      [sql|
          WITH insert_root_pg_node AS (
            INSERT INTO pg_node (
              tag,
              pg_node_parent_id,
              id,
              name,
              sql,
              pg_host,
              pg_password,
              pg_port,
              pg_user,
              pg_dbname
            )
            VALUES ('PgFile', NULL, ?, ?, ?, ?, ?, ?, ?, ?)
          ) INSERT INTO pg_collection_to_pg_node (
              pg_collection_id,
              pg_actor_id
            )
            VALUES (?, ?)
          |]


-- * insert pg file


insertPgFile :: NewPgFile -> PG.Connection -> IO ()
insertPgFile newPgFile connection =
  Monad.void $ PG.execute connection rawQuery newPgFile
  where
    rawQuery =
      [sql|
          INSERT INTO pg_node (
            tag,
            id,
            pg_node_parent_id,
            name,
            sql,
            pg_host,
            pg_password,
            pg_port,
            pg_user,
            pg_dbname
          )
          VALUES ('PgFile', ?, ?, ?, ?, ?, ?, ?, ?, ?)
          |]


-- * insert root pg folder


insertRootPgFolder :: NewRootPgFolder -> UUID -> PG.Connection -> IO ()
insertRootPgFolder NewRootPgFolder{..} pgCollectionId connection =
  Monad.void $
    PG.execute connection rawQuery ( _newRootPgFolderId
                                   , _newRootPgFolderName
                                   , pgCollectionId
                                   , _newRootPgFolderId
                                   )
  where
    rawQuery =
      [sql|
          WITH insert_root_pg_node AS (
            INSERT INTO pg_node (
              id,
              pg_node_parent_id,
              tag,
              name
            )
            VALUES (?, NULL, 'PgFolder', ?)
          ) INSERT INTO pg_collection_to_pg_node (
              pg_collection_id,
              pg_actor_id
            )
            VALUES (?, ?)
          |]


-- * insert pg folder


insertPgFolder :: NewPgFolder -> PG.Connection -> IO ()
insertPgFolder NewPgFolder {..} connection =
  Monad.void $ PG.execute connection rawQuery (_newPgFolderId, _newPgFolderParentNodeId, _newPgFolderName)
  where
    rawQuery =
      [sql|
          INSERT INTO pg_node (
            id,
            pg_node_parent_id,
            tag,
            name
          )
          VALUES (?, ?, 'PgFolder', ?)
          |]


-- * update pg file


updatePgFileDB :: UUID -> UpdatePgFile -> PG.Connection -> IO ()
updatePgFileDB pgNodeId UpdatePgFile{..} connection = do
  _ <- PG.execute connection updateQuery ( _updatePgFileName
                                         , _updatePgFileSql
                                         , _updatePgFileHost
                                         , _updatePgFilePassword
                                         , _updatePgFilePort
                                         , _updatePgFileUser
                                         , _updatePgFileDbName
                                         , pgNodeId
                                         )
  return ()
  where
    updateQuery =
      [sql|
          UPDATE pg_node
          SET
            name = ?,
            sql = ?,
            pg_host = ?,
            pg_password = ?,
            pg_port = ?,
            pg_user = ?,
            pg_dbname = ?
          WHERE id = ?
          |]
