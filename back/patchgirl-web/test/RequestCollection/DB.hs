{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module RequestCollection.DB where

import           Data.UUID
import qualified Data.UUID.V4                     as UUID
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics

import           PatchGirl.Internal


-- * new fake request collection


insertFakeRequestCollection :: UUID -> Connection -> IO Int
insertFakeRequestCollection accountId connection = do
  [Only id] <- query connection rawQuery (Only accountId)
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO request_collection (account_id)
          VALUES (?)
          RETURNING id
          |]


-- * insert request collection to request node


data NewFakeRequestCollectionToRequestNode =
  NewFakeRequestCollectionToRequestNode { _fakeRequestCollectionToRequestNodeRequestCollectionId :: Int
                                        , _fakeRequestCollectionToRequestNodeRequestRequestNodeId :: UUID
                                        }
  deriving (Eq, Show, Read, Generic, ToRow)

insertFakeRequestCollectionToRequestNode :: NewFakeRequestCollectionToRequestNode -> Connection -> IO ()
insertFakeRequestCollectionToRequestNode fakeRequestCollectionToRequestNode connection = do
  _ <- execute connection rawQuery fakeRequestCollectionToRequestNode
  return ()
  where
    rawQuery =
      [sql|
          INSERT INTO request_collection_to_request_node (request_collection_id, request_node_id)
          VALUES (?, ?)
          |]


-- * insert fake request folder


data NewFakeRequestFolder =
  NewFakeRequestFolder { _newFakeRequestFolderId       :: UUID,
                         _newFakeRequestFolderParentId :: Maybe UUID
                       , _newFakeRequestName           :: String
                       }
  deriving (Eq, Show, Read, Generic, ToRow)


insertFakeRequestFolder :: NewFakeRequestFolder -> Connection -> IO UUID
insertFakeRequestFolder fakeRequestFolder connection = do
  [Only id] <- query connection rawQuery fakeRequestFolder
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO request_node (id, request_node_parent_id, tag, name)
          VALUES (?, ?, 'RequestFolder', ?)
          RETURNING id;
          |]


-- * insert fake request file


data NewFakeRequestFile =
  NewFakeRequestFile { _fakeRequestFileId         :: UUID
                     , _fakeRequestFileParentId   :: Maybe UUID
                     , _fakeRequestFileName       :: String
                     , _fakeRequestFileHttpUrl    :: String
                     , _fakeRequestFileHttpMethod :: String
                     , _fakeRequestFileHttpBody   :: String
                     }
  deriving (Eq, Show, Read, Generic, ToRow)


insertFakeRequestFile :: NewFakeRequestFile -> Connection -> IO UUID
insertFakeRequestFile newFakeRequestFile connection = do
  [Only id] <- query connection rawQuery newFakeRequestFile
  return id
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
            http_body,
            http_headers
          ) VALUES (?, ?, 'RequestFile', ?,?,?,?, '{}')
          RETURNING id;
          |]


-- * insert sample request collection


{-
  insert a collection that looks like this:


       1     2
      / \
     3   4
    / \
   5   6


-}

insertSampleRequestCollection :: UUID -> Connection -> IO RequestCollection
insertSampleRequestCollection accountId connection = do
  n1Id <- UUID.nextRandom >>= \id -> insertFakeRequestFolder (n1 id) connection
  n2Id <- UUID.nextRandom >>= \id -> insertFakeRequestFolder (n2 id) connection
  n3Id <- UUID.nextRandom >>= \id -> insertFakeRequestFolder (n3 id n1Id) connection
  _ <- UUID.nextRandom >>= \id -> insertFakeRequestFile (n4 id n1Id) connection
  _ <- UUID.nextRandom >>= \id -> insertFakeRequestFile (n5 id n3Id) connection
  _ <- UUID.nextRandom >>= \id -> insertFakeRequestFile (n6 id n3Id) connection
  requestCollectionId <- insertFakeRequestCollection accountId connection
  let fakeRequestCollectionToRequestNode1 =
        NewFakeRequestCollectionToRequestNode { _fakeRequestCollectionToRequestNodeRequestCollectionId = requestCollectionId
                                              , _fakeRequestCollectionToRequestNodeRequestRequestNodeId = n1Id
                                              }
  let fakeRequestCollectionToRequestNode2 =
        NewFakeRequestCollectionToRequestNode { _fakeRequestCollectionToRequestNodeRequestCollectionId = requestCollectionId
                                              , _fakeRequestCollectionToRequestNodeRequestRequestNodeId = n2Id
                                              }

  _ <- insertFakeRequestCollectionToRequestNode fakeRequestCollectionToRequestNode1 connection
  _ <- insertFakeRequestCollectionToRequestNode fakeRequestCollectionToRequestNode2 connection
  requestNodes <- selectRequestNodesFromRequestCollectionId requestCollectionId connection
  return $
    RequestCollection requestCollectionId requestNodes

  where

-- ** level 1

    n1 id = NewFakeRequestFolder { _newFakeRequestFolderId = id
                                 , _newFakeRequestFolderParentId = Nothing
                                 , _newFakeRequestName           = "1/"
                                 }

    n2 id = NewFakeRequestFolder { _newFakeRequestFolderId = id
                                 , _newFakeRequestFolderParentId = Nothing
                                 , _newFakeRequestName           = "2/"
                                 }
-- ** level 2

    n3 id parentId = NewFakeRequestFolder { _newFakeRequestFolderId = id
                                          , _newFakeRequestFolderParentId = Just parentId
                                          , _newFakeRequestName           = "3/"
                                          }

    n4 id parentId = NewFakeRequestFile { _fakeRequestFileId = id
                                     , _fakeRequestFileParentId = Just parentId
                                     , _fakeRequestFileName       = "4"
                                     , _fakeRequestFileHttpUrl    = "http://4.com"
                                     , _fakeRequestFileHttpMethod = "Get"
                                     , _fakeRequestFileHttpBody   = ""
                                     }

-- ** level 3

    n5 id parentId = NewFakeRequestFile { _fakeRequestFileId = id
                                        , _fakeRequestFileParentId = Just parentId
                                        , _fakeRequestFileName       = "5"
                                        , _fakeRequestFileHttpUrl    = "http://5.com"
                                        , _fakeRequestFileHttpMethod = "Get"
                                        , _fakeRequestFileHttpBody   = ""
                                        }

    n6 id parentId = NewFakeRequestFile { _fakeRequestFileId = id
                                        , _fakeRequestFileParentId = Just parentId
                                        , _fakeRequestFileName       = "6"
                                        , _fakeRequestFileHttpUrl    = "http://6.com"
                                        , _fakeRequestFileHttpMethod = "Get"
                                        , _fakeRequestFileHttpBody   = ""
                                        }
