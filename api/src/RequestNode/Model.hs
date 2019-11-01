{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module RequestNode.Model where

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
import           Database.PostgreSQL.Simple.FromField
import           Data.Aeson (Value, parseJSON)
import Data.Aeson.Types (parseEither)

data RequestNode
  = RequestFolder { id :: Int
                  , name :: String
                  , children :: [RequestNode]
                  }
  | RequestFile { id :: Int
                , name :: String
                , url :: String
                , method :: Method
                , headers :: [(String, String)]
                , body :: String
                }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance FromField [RequestNode] where
  fromField field mdata = do
    value <- fromField field mdata :: Conversion Value
    let errorOrRequestNodes = (parseEither parseJSON) value :: Either String [RequestNode]
    either (returnError ConversionFailed field) return errorOrRequestNodes

data ParentNodeId
  = RequestCollectionId Int
  | RequestNodeId Int
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data NewRequestFile =
  NewRequestFile { newRequestFileName :: String
                 , newRequestFileParentNodeId :: ParentNodeId
                 , newRequestFileMethod :: Method
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

data NewRequestFolder =
  NewRequestFolder { newRequestFolderName :: String
                   , newRequestFolderParentNodeId :: ParentNodeId
                   } deriving (Eq, Show, Generic)

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
