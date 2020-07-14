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
                                 , _newRootScenarioFileEnvironmentId
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
              name,
              environment_id
            )
            VALUES (?, NULL, 'ScenarioFile', ?, ?)
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
                                 , _newScenarioFileEnvironmentId
                                 )
  where
    rawQuery =
      [sql|
          INSERT INTO scenario_node (
            id,
            scenario_node_parent_id,
            name,
            tag,
            environment_id
          )
          VALUES (?, ?, ?, 'ScenarioFile', ?)
          |]


-- * update scenario file


updateScenarioFileDB :: UpdateScenarioFile -> UUID -> PG.Connection -> IO Int.Int64
updateScenarioFileDB UpdateScenarioFile{..} accountId connection =
  PG.execute connection updateQuery ( accountId
                                    , _updateScenarioFileEnvironmentId
                                    , _updateScenarioFileId
                                    )
  where
    updateQuery =
      [sql|
          UPDATE scenario_node
          SET environment_id = (
            SELECT environment_id
            FROM account_environment
            WHERE account_id = ?
            AND environment_id = ?
          )
          WHERE id = ?
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
insertScene scenarioNodeId NewScene{..} connection = do
  let (httpSceneId, pgSceneId) = case _newSceneActorType of
        HttpActor ->
          (Just _newSceneActorId, Nothing)

        PgActor ->
          (Nothing, Just _newSceneActorId)

  case _newSceneSceneNodeParentId of
    Nothing ->
      PG.execute connection insertRootSceneRawQuery ( _newSceneSceneNodeParentId
                                                    , _newSceneId
                                                    , _newScenePrescript
                                                    , _newScenePostscript
                                                    , _newSceneActorType
                                                    , httpSceneId
                                                    , pgSceneId
                                                    , scenarioNodeId
                                                    )
    Just sceneNodeParentId ->
      PG.execute connection insertSceneRawQuery ( sceneNodeParentId
                                                , _newSceneId
                                                , _newScenePrescript
                                                , _newScenePostscript
                                                , _newSceneActorType
                                                , httpSceneId
                                                , pgSceneId
                                                )
  where
    insertRootSceneRawQuery =
      [sql|
          WITH new_scene AS (
            INSERT INTO scene_node (
              scene_node_parent_id,
              id,
              prescript,
              postscript,
              actor_type,
              http_actor_id,
              pg_actor_id
             )
             VALUES (?, ?, ?, ?, ?, ?, ?)
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
              scene_node_parent_id,
              id,
              prescript,
              postscript,
              actor_type,
              http_actor_id,
              pg_actor_id
             )
             VALUES (?, ?, ?, ?, ?, ?, ?)
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


-- * update scene


updateSceneDB :: UUID -> UpdateScene -> PG.Connection -> IO Int.Int64
updateSceneDB sceneId UpdateScene{..} connection =
  PG.execute connection updateQuery ( _updateScenePrescript
                                    , _updateScenePostscript
                                    , sceneId
                                    )
  where
    updateQuery =
      [sql|
          UPDATE scene_node
          SET prescript = ?, postscript = ?
          WHERE id = ?
          |]
