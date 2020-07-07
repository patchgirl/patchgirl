{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module PgCollection.DB where

import           Data.UUID
import qualified Data.UUID.V4                     as UUID
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics

import           PatchGirl.Internal


-- * new fake pg collection


insertFakePgCollection :: UUID -> Connection -> IO UUID
insertFakePgCollection accountId connection = do
  [Only id] <- query connection rawQuery (Only accountId)
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO pg_collection (account_id)
          VALUES (?)
          RETURNING id
          |]


-- * insert pg collection to pg node


data NewFakePgCollectionToPgNode =
  NewFakePgCollectionToPgNode { _fakePgCollectionToPgNodePgCollectionId :: UUID
                              , _fakePgCollectionToPgNodePgPgNodeId     :: UUID
                              }
  deriving (Eq, Show, Read, Generic, ToRow)

insertFakePgCollectionToPgNode :: NewFakePgCollectionToPgNode -> Connection -> IO ()
insertFakePgCollectionToPgNode fakePgCollectionToPgNode connection = do
  _ <- execute connection rawQuery fakePgCollectionToPgNode
  return ()
  where
    rawQuery =
      [sql|
          INSERT INTO pg_collection_to_pg_node (pg_collection_id, pg_node_id)
          VALUES (?, ?)
          |]


-- * insert fake pg folder


data NewFakePgFolder =
  NewFakePgFolder { _newFakePgFolderId       :: UUID
                  , _newFakePgFolderParentId :: Maybe UUID
                  , _newFakePgName           :: String
                  }
  deriving (Eq, Show, Read, Generic, ToRow)


insertFakePgFolder :: NewFakePgFolder -> Connection -> IO UUID
insertFakePgFolder fakePgFolder connection = do
  [Only id] <- query connection rawQuery fakePgFolder
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO pg_node (id, pg_node_parent_id, tag, name)
          VALUES (?, ?, 'PgFolder', ?)
          RETURNING id;
          |]


-- * insert fake pg file


data NewFakePgFile =
  NewFakePgFile { _fakePgFileId       :: UUID
                , _fakePgFileParentId :: Maybe UUID
                , _fakePgFileName     :: String
                , _fakePgFileSql      :: String
                }
  deriving (Eq, Show, Read, Generic, ToRow)


insertFakePgFile :: NewFakePgFile -> Connection -> IO UUID
insertFakePgFile newFakePgFile connection = do
  [Only id] <- query connection rawQuery newFakePgFile
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO pg_node (
            id,
            pg_node_parent_id,
            tag,
            name,
            sql
          ) VALUES (?, ?, 'PgFile', ?,?)
          RETURNING id;
          |]


-- * insert sample pg collection


{-
  insert a collection that looks like this:


       1     2
      / \
     3   4
    / \
   5   6


-}

insertSamplePgCollection :: UUID -> Connection -> IO PgCollection
insertSamplePgCollection accountId connection = do
  n1Id <- UUID.nextRandom >>= \id -> insertFakePgFolder (n1 id) connection
  n2Id <- UUID.nextRandom >>= \id -> insertFakePgFolder (n2 id) connection
  n3Id <- UUID.nextRandom >>= \id -> insertFakePgFolder (n3 id n1Id) connection
  n4Id <- UUID.nextRandom >>= \id -> insertFakePgFile (n4 id n1Id) connection
  n5Id <- UUID.nextRandom >>= \id -> insertFakePgFile (n5 id n3Id) connection
  n6Id <- UUID.nextRandom >>= \id -> insertFakePgFile (n6 id n3Id) connection
  pgCollectionId <- insertFakePgCollection accountId connection
  let fakePgCollectionToPgNode1 =
        NewFakePgCollectionToPgNode { _fakePgCollectionToPgNodePgCollectionId = pgCollectionId
                                    , _fakePgCollectionToPgNodePgPgNodeId = n1Id
                                    }
  let fakePgCollectionToPgNode2 =
        NewFakePgCollectionToPgNode { _fakePgCollectionToPgNodePgCollectionId = pgCollectionId
                                              , _fakePgCollectionToPgNodePgPgNodeId = n2Id
                                              }

  _ <- insertFakePgCollectionToPgNode fakePgCollectionToPgNode1 connection
  _ <- insertFakePgCollectionToPgNode fakePgCollectionToPgNode2 connection
  pgNodes <- selectPgNodesFromPgCollectionId pgCollectionId connection
  return $
    PgCollection pgCollectionId pgNodes

  where

-- ** level 1

    n1 id = NewFakePgFolder { _newFakePgFolderId = id
                            , _newFakePgFolderParentId = Nothing
                            , _newFakePgName           = "1/"
                            }

    n2 id = NewFakePgFolder { _newFakePgFolderId = id
                            , _newFakePgFolderParentId = Nothing
                            , _newFakePgName           = "2/"
                            }
-- ** level 2

    n3 id parentId = NewFakePgFolder { _newFakePgFolderId = id
                                     , _newFakePgFolderParentId = Just parentId
                                     , _newFakePgName           = "3/"
                                     }

    n4 id parentId = NewFakePgFile { _fakePgFileId = id
                                   , _fakePgFileParentId = Just parentId
                                   , _fakePgFileName       = "4"
                                   , _fakePgFileSql = ""
                                   }

-- ** level 3

    n5 id parentId = NewFakePgFile { _fakePgFileId = id
                                   , _fakePgFileParentId = Just parentId
                                   , _fakePgFileName       = "5"
                                   , _fakePgFileSql = ""
                                   }

    n6 id parentId = NewFakePgFile { _fakePgFileId = id
                                   , _fakePgFileParentId = Just parentId
                                   , _fakePgFileName       = "6"
                                   , _fakePgFileSql = ""
                                   }
