{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module ScenarioNode.DB where

import qualified Data.Maybe                       as Maybe
import           Data.UUID
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics

import           ScenarioNode.Model


-- * select fake scenario file


data FakeScenarioFile =
  FakeScenarioFile { _fakeScenarioFileParentId    :: Maybe UUID
                   , _fakeScenarioFileName        :: String
                   , _fakeScenarioFileSceneNodeId :: Maybe UUID
                   }
  deriving (Eq, Show, Read, Generic, FromRow)


selectFakeScenarioFile :: UUID -> Connection -> IO FakeScenarioFile
selectFakeScenarioFile id connection = do
  [fakeScenarioFile] <- query connection rawQuery (Only id)
  return fakeScenarioFile
  where
    rawQuery =
      [sql|
          SELECT scenario_node_parent_id, name, scene_node_id
          FROM scenario_node
          WHERE id = ?
          AND tag = 'ScenarioFile'
          |]


-- * select fake scenario folder


data FakeScenarioFolder =
  FakeScenarioFolder { _fakeScenarioFolderParentId :: Maybe UUID
                     , _fakeScenarioFolderName     :: String
                     }
  deriving (Eq, Show, Read, Generic, FromRow)


selectFakeScenarioFolder :: UUID -> Connection -> IO FakeScenarioFolder
selectFakeScenarioFolder scenarioNodeId connection = do
  [fakeScenarioFolder] <- query connection rawQuery (Only scenarioNodeId)
  return fakeScenarioFolder
  where
    rawQuery =
      [sql|
          SELECT scenario_node_parent_id, name
          FROM scenario_node
          where id = ?
          |]


-- * select scenario node exist


selectNodeExists :: UUID -> Connection -> IO Bool
selectNodeExists id connection = do
  [Only nodeExists] <- query connection rawQuery (Only id)
  return nodeExists
  where
    rawQuery =
      [sql|
          SELECT EXISTS (
            SELECT 1
            FROM scenario_node
            WHERE id = ?
          )
          |]



-- * util


getFirstScenarioFolder :: [ScenarioNode] -> Maybe ScenarioNode
getFirstScenarioFolder scenarioNodes =
  Maybe.listToMaybe . Maybe.catMaybes $ map findFolder scenarioNodes
  where
    findFolder :: ScenarioNode -> Maybe ScenarioNode
    findFolder = \case
      folder@ScenarioFolder {} -> Just folder
      _ -> Nothing


getFirstScenarioFile :: [ScenarioNode] -> Maybe ScenarioNode
getFirstScenarioFile scenarioNodes =
  Maybe.listToMaybe . Maybe.catMaybes $ map findFile scenarioNodes
  where
    findFile :: ScenarioNode -> Maybe ScenarioNode
    findFile = \case
      file@ScenarioFile {} -> Just file
      ScenarioFolder { _scenarioNodeChildren } ->
        getFirstScenarioFile _scenarioNodeChildren
