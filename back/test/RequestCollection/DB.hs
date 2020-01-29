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


-- * new fake request collection


data FakeRequestCollection =
  FakeRequestCollection { _fakeRequestCollectionId        :: Int
                        , _fakeRequestCollectionAccountId :: Int
                        }
  deriving (Eq, Show, Read, Generic, FromRow)

insertFakeRequestCollection :: Int -> Connection -> IO FakeRequestCollection
insertFakeRequestCollection accountId connection = do
  [fakeRequestCollection] <- query connection rawQuery (Only accountId)
  return fakeRequestCollection
  where
    rawQuery =
      [sql|
          INSERT INTO request_collection (account_id)
          VALUES (?)
          RETURNING id, account_id
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
          INSERT INTO request_collection_to_request_node (request_collection_id, request_node_id)
          VALUES (?, ?)
          |]


-- * insert fake request folder


data FakeRequestFolder =
  FakeRequestFolder { _fakeRequestFolderId       :: Int
                    , _fakeRequestFolderParentId :: Maybe Int
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
          INSERT INTO request_node (id, request_node_parent_id, tag, name)
          VALUES (?, ?, 'RequestFolder', ?)
          RETURNING id;
          |]


-- * insert fake request file


data FakeRequestFile =
  FakeRequestFile { _fakeRequestFileId         :: Int
                  , _fakeRequestFileParentId   :: Maybe Int
                  , _fakeRequestFileName       :: String
                  , _fakeRequestFileHttpUrl    :: String
                  , _fakeRequestFileHttpMethod :: String
                  , _fakeRequestFileHttpBody   :: String
                  }
  deriving (Eq, Show, Read, Generic, ToRow)


insertFakeRequestFile :: FakeRequestFile -> Connection -> IO ()
insertFakeRequestFile fakeRequestFile connection = do
  _ <- execute connection rawQuery fakeRequestFile
  return ()
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
          ) values (?,?, 'RequestFile', ?,?,?,?, '{}');
          |]
