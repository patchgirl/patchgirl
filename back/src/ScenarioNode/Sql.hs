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
