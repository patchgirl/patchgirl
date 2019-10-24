{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module RequestNode where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           GHC.Generics
import           Servant (Handler)
import           Control.Monad.IO.Class (liftIO)
import           DB
import Http
import           Data.Aeson (FromJSON, ToJSON)

data ParentNodeId
  = RequestCollectionId Int
  | RequestNodeId Int
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- * create file

data NewRequestFile =
  NewRequestFile { newRequestFileName :: String
                 , newRequestFileParentNodeId :: ParentNodeId
                 , newRequestFileMethod :: Method
                 } deriving (Eq, Show, Generic, FromJSON, ToJSON)

data CreatedRequestFile =
  CreatedRequestFile { createdRequestFileId :: Int
                     , createdRequestFileName :: String
                     , createdRequestFileCollectionId :: Maybe Int
                     , createdRequestFileParentId :: Maybe Int
                     , createdRequestFileHttpMethod :: Method
                     } deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance ToRow NewRequestFile where
  toRow (NewRequestFile { newRequestFileName
                        , newRequestFileParentNodeId
                        , newRequestFileMethod
                        }) =
    let
      tag = "RequestFile" :: String
      noId = Nothing :: Maybe Int
    in
      case newRequestFileParentNodeId of
        RequestCollectionId requestCollectionId ->
          toRow ( requestCollectionId
                , noId
                , tag
                , newRequestFileName
                , newRequestFileMethod
                )
        RequestNodeId requestNodeId ->
          toRow ( noId
                , requestNodeId
                , tag
                , newRequestFileName
                , newRequestFileMethod
                )

instance FromRow CreatedRequestFile where
  fromRow = do
    createdRequestFileId <- field
    createdRequestFileName <- field
    createdRequestFileCollectionId <- field
    createdRequestFileParentId <- field
    createdRequestFileHttpMethod <- pure Get
    return CreatedRequestFile {..}

insertRequestFile :: NewRequestFile -> Connection -> IO CreatedRequestFile
insertRequestFile newRequestFile connection = do
  [createdRequestFile] <- query connection rawQuery $ newRequestFile
  return createdRequestFile
  where
    rawQuery =
      [sql|
          INSERT INTO request_node (
            request_collection_id,
            request_node_parent_id,
            tag,
            name,
            http_method
          )
          VALUES (?, ?, ?, ?, ?)
          RETURNING
            id,
            name,
            request_collection_id,
            request_node_parent_id
          |]

createRequestFile :: Int -> NewRequestFile -> Handler CreatedRequestFile
createRequestFile requestCollectionId newRequestFile = do
  liftIO (getDBConnection >>= (insertRequestFile newRequestFile)) >>= return

updateRequestFile :: Int -> Int -> Handler CreatedRequestFile
updateRequestFile requestCollectionId requestFileId =
  undefined

-- * create folder

data NewRequestFolder =
  NewRequestFolder { newRequestFolderName :: String
                   , newRequestFolderParentNodeId :: ParentNodeId
                   } deriving (Eq, Show, Generic)

data CreatedRequestFolder =
  CreatedRequestFolder { requestFolderId :: Int
                       , requestFolderName :: String
                       , requestFolderCollectionId :: Maybe Int
                       , requestFolderParentId :: Maybe Int
                       }

instance ToRow NewRequestFolder where
  toRow (NewRequestFolder { newRequestFolderName
                          , newRequestFolderParentNodeId
                          }) =
    let
      tag = "RequestFolder" :: String
      noId = Nothing :: Maybe Int
    in
      case newRequestFolderParentNodeId of
        RequestCollectionId requestCollectionId ->
          toRow ( requestCollectionId
                , noId
                , tag
                , newRequestFolderName
                )
        RequestNodeId requestNodeId ->
          toRow ( noId
                , requestNodeId
                , tag
                , newRequestFolderName
                )

instance FromRow CreatedRequestFolder where
  fromRow = do
    requestFolderId <- field
    requestFolderName <- field
    requestFolderCollectionId <- field
    requestFolderParentId <- field
    return CreatedRequestFolder {..}

insertRequestFolder :: NewRequestFolder -> Connection -> IO CreatedRequestFolder
insertRequestFolder newRequestFolder connection = do
  [createdRequestFolder] <- query connection rawQuery $ newRequestFolder
  return createdRequestFolder
  where
    rawQuery =
      [sql|
          INSERT INTO request_node (
            request_collection_id,
            request_node_parent_id,
            tag,
            name
          )
          VALUES (?, ?, ?, ?)
          RETURNING
            id,
            name,
            request_collection_id,
            request_node_parent_id
          |]
