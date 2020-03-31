{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module ScenarioCollection.DB where

import qualified Control.Monad                    as Monad
import           Data.UUID
import qualified Data.UUID.V4                     as UUID
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics
import           ScenarioCollection.Model
import           ScenarioNode.Sql


-- * new fake scenario collection


insertFakeScenarioCollection :: UUID -> Connection -> IO UUID
insertFakeScenarioCollection accountId connection = do
  [Only id] <- query connection rawQuery (Only accountId)
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO scenario_collection (id, account_id)
          VALUES (uuid_generate_v4(), ?)
          RETURNING id
          |]


-- * insert scenario collection to scenario node


data NewFakeScenarioCollectionToScenarioNode =
  NewFakeScenarioCollectionToScenarioNode { _fakeScenarioCollectionToScenarioNodeScenarioCollectionId :: UUID
                                          , _fakeScenarioCollectionToScenarioNodeScenarioNodeId :: UUID
                                          }
  deriving (Eq, Show, Read, Generic, ToRow)

insertFakeScenarioCollectionToScenarioNode :: NewFakeScenarioCollectionToScenarioNode -> Connection -> IO ()
insertFakeScenarioCollectionToScenarioNode fakeScenarioCollectionToScenarioNode connection = do
  _ <- execute connection rawQuery fakeScenarioCollectionToScenarioNode
  return ()
  where
    rawQuery =
      [sql|
          INSERT INTO scenario_collection_to_scenario_node (scenario_collection_id, scenario_node_id)
          VALUES (?, ?)
          |]


-- * insert fake scenario folder


data NewFakeScenarioFolder =
  NewFakeScenarioFolder { _newFakeScenarioFolderId       :: UUID,
                          _newFakeScenarioFolderParentId :: Maybe UUID
                        , _newFakeScenarioName           :: String
                        }
  deriving (Eq, Show, Read, Generic, ToRow)


insertFakeScenarioFolder :: NewFakeScenarioFolder -> Connection -> IO UUID
insertFakeScenarioFolder fakeScenarioFolder connection = do
  [Only id] <- query connection rawQuery fakeScenarioFolder
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO scenario_node (id, scenario_node_parent_id, tag, name)
          VALUES (?, ?, 'ScenarioFolder', ?)
          RETURNING id;
          |]


-- * insert fake scenario file


data NewFakeScenarioFile =
  NewFakeScenarioFile { _fakeScenarioFileId       :: UUID
                      , _fakeScenarioFileParentId :: Maybe UUID
                      , _fakeScenarioFileName     :: String
                      , _fakeScenarioFileSceneId  :: Maybe UUID
                      }
  deriving (Eq, Show, Read, Generic, ToRow)


insertFakeScenarioFile :: NewFakeScenarioFile -> Connection -> IO UUID
insertFakeScenarioFile newFakeScenarioFile connection = do
  [Only id] <- query connection rawQuery newFakeScenarioFile
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO scenario_node (
            id,
            scenario_node_parent_id,
            tag,
            name,
            scene_node_id
          ) VALUES (?, ?, 'ScenarioFile', ?,?)
          RETURNING id;
          |]


-- * insert sample scenario collection


{-
  insert a collection that looks like this:


       1     2
      / \
     3   4
    / \
   5   6


-}

insertSampleScenarioCollection :: UUID -> Connection -> IO ScenarioCollection
insertSampleScenarioCollection accountId connection = do
  n1Id <- UUID.nextRandom >>= \id -> insertFakeScenarioFolder (n1 id) connection
  n2Id <- UUID.nextRandom >>= \id -> insertFakeScenarioFolder (n2 id) connection
  n3Id <- UUID.nextRandom >>= \id -> insertFakeScenarioFolder (n3 id n1Id) connection
  _ <- UUID.nextRandom >>= \id -> insertFakeScenarioFile (n4 id n1Id) connection
  _ <- UUID.nextRandom >>= \id -> insertFakeScenarioFile (n5 id n3Id) connection
  _ <- UUID.nextRandom >>= \id -> insertFakeScenarioFile (n6 id n3Id) connection
  scenarioCollectionId <- insertFakeScenarioCollection accountId connection
  let fakeScenarioCollectionToScenarioNode1 =
        NewFakeScenarioCollectionToScenarioNode { _fakeScenarioCollectionToScenarioNodeScenarioCollectionId = scenarioCollectionId
                                                , _fakeScenarioCollectionToScenarioNodeScenarioNodeId = n1Id
                                                }
  let fakeScenarioCollectionToScenarioNode2 =
        NewFakeScenarioCollectionToScenarioNode { _fakeScenarioCollectionToScenarioNodeScenarioCollectionId = scenarioCollectionId
                                                , _fakeScenarioCollectionToScenarioNodeScenarioNodeId = n2Id
                                                }
  _ <- insertFakeScenarioCollectionToScenarioNode fakeScenarioCollectionToScenarioNode1 connection
  _ <- insertFakeScenarioCollectionToScenarioNode fakeScenarioCollectionToScenarioNode2 connection
  scenarioNodes <- selectScenarioNodesFromScenarioCollectionId scenarioCollectionId connection
  return $
    ScenarioCollection scenarioCollectionId scenarioNodes

  where

-- ** level 1

    n1 id = NewFakeScenarioFolder { _newFakeScenarioFolderId = id
                                 , _newFakeScenarioFolderParentId = Nothing
                                 , _newFakeScenarioName           = "1/"
                                 }

    n2 id = NewFakeScenarioFolder { _newFakeScenarioFolderId = id
                                 , _newFakeScenarioFolderParentId = Nothing
                                 , _newFakeScenarioName           = "2/"
                                 }
-- ** level 2

    n3 id parentId = NewFakeScenarioFolder { _newFakeScenarioFolderId = id
                                          , _newFakeScenarioFolderParentId = Just parentId
                                          , _newFakeScenarioName           = "3/"
                                          }

    n4 id parentId = NewFakeScenarioFile { _fakeScenarioFileId = id
                                         , _fakeScenarioFileParentId = Just parentId
                                         , _fakeScenarioFileName       = "4"
                                         , _fakeScenarioFileSceneId   = Nothing
                                         }

-- ** level 3

    n5 id parentId = NewFakeScenarioFile { _fakeScenarioFileId = id
                                         , _fakeScenarioFileParentId = Just parentId
                                         , _fakeScenarioFileName       = "5"
                                         , _fakeScenarioFileSceneId   = Nothing
                                         }

    n6 id parentId = NewFakeScenarioFile { _fakeScenarioFileId = id
                                         , _fakeScenarioFileParentId = Just parentId
                                         , _fakeScenarioFileName       = "6"
                                         , _fakeScenarioFileSceneId   = Nothing
                                         }
