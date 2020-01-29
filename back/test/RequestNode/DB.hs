{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module RequestNode.DB where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics


-- * select fake request file


data FakeRequestFile =
  FakeRequestFile { _fakeRequestFileParentId   :: Maybe Int
                  , _fakeRequestFileName       :: String
                  , _fakeRequestFileHttpUrl    :: String
                  , _fakeRequestFileHttpMethod :: String
                  , _fakeRequestFileHttpBody   :: String
                  }
  deriving (Eq, Show, Read, Generic, FromRow)


selectFakeRequestFile :: Int -> Connection -> IO FakeRequestFile
selectFakeRequestFile fakeRequestFileId connection = do
  [fakeRequestFile] <- query connection rawQuery (Only fakeRequestFileId)
  return fakeRequestFile
  where
    rawQuery =
      [sql|
          SELECT id, name, http_url, http_method, http_body
          FROM request_node
          where id = ?
          |]


-- * select fake request folder


data FakeRequestFolder =
  FakeRequestFolder { _fakeRequestFolderName :: String
                    }
  deriving (Eq, Show, Read, Generic, FromRow)


selectFakeRequestFolder :: Int -> Connection -> IO FakeRequestFolder
selectFakeRequestFolder fakeRequestFolderId connection = do
  [fakeRequestFolder] <- query connection rawQuery (Only fakeRequestFolderId)
  return fakeRequestFolder
  where
    rawQuery =
      [sql|
          SELECT name
          FROM request_node
          where id = ?
          |]
