

module ScenarioNode.Sql where

import           Control.Lens.Getter              ((^.))
import           Data.UUID
import qualified Database.PostgreSQL.Simple       as PG
import           Database.PostgreSQL.Simple.SqlQQ
import qualified GHC.Int                          as Int

import           ScenarioCollection.Sql
import           ScenarioNode.Model


-- * select scenario nodes from account id


selectScenarioNodesFromAccountId :: UUID -> PG.Connection -> IO [ScenarioNode]
selectScenarioNodesFromAccountId accountId connection =
  selectScenarioCollectionId accountId connection >>= \case
    Nothing -> pure []
    Just scenarioCollectionId ->
      selectScenarioNodesFromScenarioCollectionId scenarioCollectionId connection


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


-- * insert scene


insertScene :: UUID -> NewScene -> PG.Connection -> IO Int.Int64
insertScene scenarioNodeId NewScene {..} connection =
  case _newSceneSceneNodeParentId of
    Nothing ->
      PG.execute connection insertRootSceneRawQuery ( _newSceneId
                                                    , _newSceneRequestFileNodeId
                                                    , scenarioNodeId
                                                    )
    Just sceneNodeParentId ->
      PG.execute connection insertSceneRawQuery ( _newSceneId
                                                , sceneNodeParentId
                                                , _newSceneRequestFileNodeId
                                                )
  where
    insertRootSceneRawQuery =
      [sql|
          WITH new_scene AS (
            INSERT INTO scene_node (
              id,
              scene_node_parent_id,
              request_node_id
             )
             VALUES (?, NULL, ?)
             RETURNING id
          ), current_scenario_node AS(
            SELECT id, scene_node_id
            FROM scenario_node
            WHERE id = ?
          ), son_scene AS (
            UPDATE scene_node
            SET scene_node_parent_id = (SELECT id FROM new_scene)
            WHERE id = (SELECT scene_node_id FROM current_scenario_node)
          ) UPDATE scenario_node
            SET scene_node_id = (SELECT id FROM new_scene)
            WHERE id = (SELECT id FROM current_scenario_node)
          |]

    insertSceneRawQuery =
      [sql|
          WITH new_scene AS (
            INSERT INTO scene_node (
              id,
              scene_node_parent_id,
              request_node_id
             )
             VALUES (?, ?, ?)
             RETURNING id, scene_node_parent_id
          ) UPDATE scene_node
            SET scene_node_parent_id = (SELECT id FROM new_scene)
            WHERE scene_node_parent_id = (SELECT scene_node_parent_id FROM new_scene)
          |]


-- * delete scene


deleteScene :: UUID -> PG.Connection -> IO Int.Int64
deleteScene sceneId connection =
  PG.execute connection updateQuery (PG.Only sceneId)
  where
    updateQuery =
      [sql|
          WITH delete_scene AS (
            DELETE FROM scene_node
            WHERE id = ?
            RETURNING id, scene_node_parent_id
          ), update_son_scene AS(
            UPDATE scene_node
            SET scene_node_parent_id = (SELECT scene_node_parent_id FROM delete_scene)
            WHERE scene_node_parent_id = (SELECT id FROM delete_scene)
            RETURNING id
          ) UPDATE scenario_node
            SET scene_node_id = (SELECT id FROM update_son_scene)
            WHERE scene_node_id = (SELECT id FROM delete_scene)
          |]
