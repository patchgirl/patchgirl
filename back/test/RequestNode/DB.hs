{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module RequestNode.DB where

import qualified Data.ByteString                      as BS
import qualified Data.ByteString.UTF8                 as BSU
import qualified Data.Maybe                           as Maybe
import           Data.UUID
import           Database.PostgreSQL.Simple
import qualified Database.PostgreSQL.Simple           as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import           Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple.Types     as PG
import           GHC.Generics

import           Http
import           RequestNode.Model


-- * select fake request file


data FakeRequestFile =
  FakeRequestFile { _fakeRequestFileParentId    :: Maybe UUID
                  , _fakeRequestFileName        :: String
                  , _fakeRequestFileHttpUrl     :: String
                  , _fakeRequestFileHttpMethod  :: Method
                  , _fakeRequestFileHttpHeaders :: [HttpHeader]
                  , _fakeRequestFileHttpBody    :: String
                  }
  deriving (Eq, Show, Read, Generic, FromRow)

instance PG.FromField [HttpHeader] where
  fromField field mdata = do
    PG.PGArray httpHeaders <- PG.fromField field mdata :: PG.Conversion (PG.PGArray HttpHeader)
    return httpHeaders

{-
 this instance will fail if either the header key or header value contains a (,) or (")
-}
instance PG.FromField HttpHeader where
  fromField _ = \case
    Nothing -> error "invalid field"
    Just bs -> do
      return $ readHttpHeader bs
    where
      readHttpHeader :: BS.ByteString -> HttpHeader
      readHttpHeader bs = -- bs should have the shape: (someHeader,someValue)
        let
          (key, value) = (read $ BSU.toString bs) :: (String, String)
        in HttpHeader (key, value)

selectFakeRequestFile :: UUID -> Connection -> IO FakeRequestFile
selectFakeRequestFile id connection = do
  [fakeRequestFile] <- query connection rawQuery (Only id)
  return fakeRequestFile
  where
    rawQuery =
      [sql|
          SELECT request_node_parent_id, name, http_url, http_method, http_headers, http_body
          FROM request_node
          where id = ?
          |]


-- * select node exist


selectNodeExists :: UUID -> Connection -> IO Bool
selectNodeExists id connection = do
  [Only nodeExists] <- query connection rawQuery (Only id)
  return nodeExists
  where
    rawQuery =
      [sql|
          SELECT EXISTS (
            SELECT 1
            FROM request_node
            WHERE id = ?
          )
          |]


-- * select fake request folder


data FakeRequestFolder =
  FakeRequestFolder { _fakeRequestFolderParentId :: Maybe UUID
                    , _fakeRequestFolderName     :: String
                    }
  deriving (Eq, Show, Read, Generic, FromRow)


selectFakeRequestFolder :: UUID -> Connection -> IO FakeRequestFolder
selectFakeRequestFolder fakeRequestFolderId connection = do
  [fakeRequestFolder] <- query connection rawQuery (Only fakeRequestFolderId)
  return fakeRequestFolder
  where
    rawQuery =
      [sql|
          SELECT request_node_parent_id, name
          FROM request_node
          where id = ?
          |]

getFirstFolder :: [RequestNode] -> Maybe RequestNode
getFirstFolder requestNodes =
  Maybe.listToMaybe . Maybe.catMaybes $ map findFolder requestNodes
  where
    findFolder :: RequestNode -> Maybe RequestNode
    findFolder = \case
      folder@RequestFolder {} -> Just folder
      _ -> Nothing

getFirstFile :: [RequestNode] -> Maybe RequestNode
getFirstFile requestNodes =
  Maybe.listToMaybe . Maybe.catMaybes $ map findFile requestNodes
  where
    findFile :: RequestNode -> Maybe RequestNode
    findFile = \case
      file@RequestFile {} -> Just file
      RequestFolder { _requestNodeChildren } ->
        getFirstFile _requestNodeChildren
