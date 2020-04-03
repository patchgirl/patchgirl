{-# LANGUAGE FlexibleContexts #-}

module ScenarioNode.Sql where

import           Control.Lens.Getter              ((^.))
import           Data.UUID
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ
import qualified GHC.Int                          as Int

import           ScenarioNode.Model


-- * select scenario nodes from scenario collection id


selectScenarioNodesFromScenarioCollectionId :: UUID -> PG.Connection -> IO [ScenarioNode]
selectScenarioNodesFromScenarioCollectionId scenarioCollectionId connection = do
    res :: [PG.Only ScenarioNodeFromPG] <- PG.query connection selectScenarioNodeSql (PG.Only scenarioCollectionId)
    return $ map (fromPgScenarioNodeToScenarioNode . (\(PG.Only r) -> r)) res
  where
    selectScenarioNodeSql =
      [sql|
          SELECT UNNEST(root_scenario_nodes_as_json(?));
          |] :: PG.Query


-- * update scenario node (rename)


updateScenarioNodeDB :: UUID -> UpdateScenarioNode -> PG.Connection -> IO ()
updateScenarioNodeDB scenarioNodeId updateScenarioNode connection = do
  let newName = updateScenarioNode ^. updateScenarioNodeName
  _ <- PG.execute connection updateQuery (newName, scenarioNodeId)
  return ()
  where
    updateQuery =
      [sql|
          UPDATE scenario_node
          SET name = ?
          WHERE id = ?
          |]


-- * delete scenario node


deleteScenarioNodeDB :: UUID -> PG.Connection -> IO Int.Int64
deleteScenarioNodeDB scenarioNodeId connection =
  PG.execute connection updateQuery (PG.Only scenarioNodeId)
  where
    updateQuery =
      [sql|
          DELETE FROM scenario_node
          WHERE id = ?
          |]


-- * insert root scenario file


insertRootScenarioFile :: NewRootScenarioFile -> UUID -> PG.Connection -> IO Int.Int64
insertRootScenarioFile NewRootScenarioFile {..} scenarioCollectionId connection =
  PG.execute connection rawQuery ( _newRootScenarioFileId
                                 , _newRootScenarioFileName
                                 , scenarioCollectionId
                                 , _newRootScenarioFileId
                                 )
  where
    rawQuery =
      [sql|
          WITH insert_root_scenario_node AS (
            INSERT INTO scenario_node (
              id,
              scenario_node_parent_id,
              tag,
              name
            )
            VALUES (?, NULL, 'ScenarioFile', ?)
          ) INSERT INTO scenario_collection_to_scenario_node (
              scenario_collection_id,
              scenario_node_id
            )
            VALUES (?, ?)
          |]


-- * insert scenario file


insertScenarioFile :: NewScenarioFile -> PG.Connection -> IO Int.Int64
insertScenarioFile NewScenarioFile {..} connection =
  PG.execute connection rawQuery ( _newScenarioFileId
                                 , _newScenarioFileParentNodeId
                                 , _newScenarioFileName
                                 )
  where
    rawQuery =
      [sql|
          INSERT INTO scenario_node (
            id,
            scenario_node_parent_id,
            name,
            tag
          )
          VALUES (?, ?, ?, 'ScenarioFile')
          |]


-- * insert root scenario folder


insertRootScenarioFolder :: NewRootScenarioFolder -> UUID -> PG.Connection -> IO Int.Int64
insertRootScenarioFolder NewRootScenarioFolder {..} scenarioCollectionId connection =
  PG.execute connection rawQuery ( _newRootScenarioFolderId
                                 , scenarioCollectionId
                                 , _newRootScenarioFolderId
                                 )
  where
    rawQuery =
      [sql|
          WITH insert_root_scenario_node AS (
            INSERT INTO scenario_node (
              id,
              scenario_node_parent_id,
              tag,
              name
            )
            VALUES (?, NULL, 'ScenarioFolder', 'new folder')
          ) INSERT INTO scenario_collection_to_scenario_node (
              scenario_collection_id,
              scenario_node_id
            )
            VALUES (?, ?)
          |]


-- * insert scenario folder


insertScenarioFolder :: NewScenarioFolder -> PG.Connection -> IO Int.Int64
insertScenarioFolder NewScenarioFolder {..} connection =
  PG.execute connection rawQuery ( _newScenarioFolderId
                                 , _newScenarioFolderParentNodeId
                                 , _newScenarioFolderName
                                 )
  where
    rawQuery =
      [sql|
          INSERT INTO scenario_node (
            id,
            scenario_node_parent_id,
            tag,
            name
          )
          VALUES (?, ?, 'ScenarioFolder', ?)
          |]
