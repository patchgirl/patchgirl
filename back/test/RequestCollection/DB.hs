{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module RequestCollection.DB where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics
import           RequestCollection.Model
import           RequestNode.Sql


-- * new fake request collection


insertFakeRequestCollection :: Int -> Connection -> IO Int
insertFakeRequestCollection accountId connection = do
  [Only id] <- query connection rawQuery (Only accountId)
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO request_collection2 (account_id)
          VALUES (?)
          RETURNING id
          |]


-- * insert request collection to request node


data FakeRequestCollectionToRequestNode =
  FakeRequestCollectionToRequestNode { _fakeRequestCollectionToRequestNodeRequestCollectionId :: Int
                                     , _fakeRequestCollectionToRequestNodeRequestRequestNodeId :: Int
                                     }
  deriving (Eq, Show, Read, Generic, ToRow)

insertFakeRequestCollectionToRequestNode :: FakeRequestCollectionToRequestNode -> Connection -> IO ()
insertFakeRequestCollectionToRequestNode fakeRequestCollectionToRequestNode connection = do
  _ <- execute connection rawQuery fakeRequestCollectionToRequestNode
  return ()
  where
    rawQuery =
      [sql|
          INSERT INTO request_collection_to_request_node2 (request_collection_id, request_node_id)
          VALUES (?, ?)
          |]


-- * insert fake request folder


data FakeRequestFolder =
  FakeRequestFolder { _fakeRequestFolderParentId :: Maybe Int
                    , _fakeRequestName           :: String
                    }
  deriving (Eq, Show, Read, Generic, ToRow)


insertFakeRequestFolder :: FakeRequestFolder -> Connection -> IO Int
insertFakeRequestFolder fakeRequestFolder connection = do
  [Only id] <- query connection rawQuery fakeRequestFolder
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO request_node (request_node_parent_id, tag, name)
          VALUES (?, 'RequestFolder', ?)
          RETURNING id;
          |]


-- * insert fake request file


data FakeRequestFile =
  FakeRequestFile { _fakeRequestFileParentId   :: Maybe Int
                  , _fakeRequestFileName       :: String
                  , _fakeRequestFileHttpUrl    :: String
                  , _fakeRequestFileHttpMethod :: String
                  , _fakeRequestFileHttpBody   :: String
                  }
  deriving (Eq, Show, Read, Generic, ToRow)


insertFakeRequestFile :: FakeRequestFile -> Connection -> IO Int
insertFakeRequestFile fakeRequestFile connection = do
  [Only id] <- query connection rawQuery fakeRequestFile
  return id
  where
    rawQuery =
      [sql|
          INSERT INTO request_node (
            request_node_parent_id,
            tag,
            name,
            http_url,
            http_method,
            http_body,
            http_headers
          ) VALUES (?, 'RequestFile', ?,?,?,?, '{}')
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

insertSampleRequestCollection :: Int -> Connection -> IO RequestCollection
insertSampleRequestCollection accountId connection = do
      n1Id <- insertFakeRequestFolder n1 connection
      n2Id <- insertFakeRequestFolder n2 connection
      n3Id <- insertFakeRequestFolder (n3 n1Id) connection
      _ <- insertFakeRequestFile (n4 n1Id) connection
      _ <- insertFakeRequestFile (n5 n3Id) connection
      _ <- insertFakeRequestFile (n6 n3Id) connection
      requestCollectionId <- insertFakeRequestCollection accountId connection
      let fakeRequestCollectionToRequestNode1 =
            FakeRequestCollectionToRequestNode { _fakeRequestCollectionToRequestNodeRequestCollectionId = requestCollectionId
                                               , _fakeRequestCollectionToRequestNodeRequestRequestNodeId = n1Id
                                               }
      let fakeRequestCollectionToRequestNode2 =
            FakeRequestCollectionToRequestNode { _fakeRequestCollectionToRequestNodeRequestCollectionId = requestCollectionId
                                               , _fakeRequestCollectionToRequestNodeRequestRequestNodeId = n2Id
                                               }

      _ <- insertFakeRequestCollectionToRequestNode fakeRequestCollectionToRequestNode1 connection
      _ <- insertFakeRequestCollectionToRequestNode fakeRequestCollectionToRequestNode2 connection
      requestNodes <- selectRequestNodesFromRequestCollectionId requestCollectionId connection
      return $
        RequestCollection requestCollectionId requestNodes
  where

-- ** level 1

    n1 = FakeRequestFolder { _fakeRequestFolderParentId = Nothing
                           , _fakeRequestName           = "1/"
                           }

    n2 = FakeRequestFolder { _fakeRequestFolderParentId = Nothing
                           , _fakeRequestName           = "2/"
                           }
-- ** level 2

    n3 id = FakeRequestFolder { _fakeRequestFolderParentId = Just id
                              , _fakeRequestName           = "3/"
                              }

    n4 id = FakeRequestFile { _fakeRequestFileParentId = Just id
                            , _fakeRequestFileName       = "4"
                            , _fakeRequestFileHttpUrl    = "http://4.com"
                            , _fakeRequestFileHttpMethod = "Get"
                            , _fakeRequestFileHttpBody   = ""
                            }

-- ** level 3

    n5 id = FakeRequestFile { _fakeRequestFileParentId = Just id
                            , _fakeRequestFileName       = "5"
                            , _fakeRequestFileHttpUrl    = "http://5.com"
                            , _fakeRequestFileHttpMethod = "Get"
                            , _fakeRequestFileHttpBody   = ""
                            }

    n6 id = FakeRequestFile { _fakeRequestFileParentId = Just id
                            , _fakeRequestFileName       = "6"
                            , _fakeRequestFileHttpUrl    = "http://6.com"
                            , _fakeRequestFileHttpMethod = "Get"
                            , _fakeRequestFileHttpBody   = ""
                            }
