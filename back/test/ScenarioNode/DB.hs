{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module ScenarioNode.DB where

import qualified Data.ByteString                      as BS
import qualified Data.ByteString.UTF8                 as BSU
import qualified Data.Maybe                           as Maybe
import qualified Data.Strings                         as Strings
import           Data.UUID
import           Database.PostgreSQL.Simple
import qualified Database.PostgreSQL.Simple.FromField as PG
import           Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple.Types     as PG
import           GHC.Generics

import           Http
import           ScenarioNode.Model


-- * select fake scenario file


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


-- * util


getFirstScenarioFile :: [ScenarioNode] -> Maybe ScenarioNode
getFirstScenarioFile scenarioNodes =
  Maybe.listToMaybe . Maybe.catMaybes $ map findFile scenarioNodes
  where
    findFile :: ScenarioNode -> Maybe ScenarioNode
    findFile = \case
      file@ScenarioFile {} -> Just file
      ScenarioFolder { _scenarioNodeChildren } ->
        getFirstScenarioFile _scenarioNodeChildren
