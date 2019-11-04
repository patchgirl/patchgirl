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
import           Data.Aeson (withObject, FromJSON(..), ToJSON, (.:))
import           Data.Aeson.Types (Parser, camelTo2, fieldLabelModifier, genericParseJSON, defaultOptions)
import           Database.PostgreSQL.Simple.FromField hiding (name)
import           Data.Aeson (Value, parseJSON)
import Data.Aeson.Types (parseEither)

data Header = Header String String

data RequestNode
  = RequestFolder { id :: Int
                  , name :: String
                  , children :: [RequestNode]
                  }
  | RequestFile { id :: Int
                , name :: String
                , httpUrl :: String
                , httpMethod :: Method
                , httpHeaders :: [(String, String)]
                , httpBody :: String
                }
  deriving (Eq, Show, Generic, ToJSON)

instance FromJSON RequestNode where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

newtype RequestNodeFromPG = RequestNodeFromPG RequestNode

data PGHeader = PGHeader { headerKey :: String
                         , headerValue :: String
                         } deriving (Eq, Show)

instance FromJSON PGHeader where
  parseJSON = withObject "PGHeader" $ \o -> do
    headerKey <- o .: "header_key"
    headerValue <- o .: "header_value"
    return PGHeader{..}

instance FromJSON RequestNodeFromPG where
  parseJSON = withObject "RequestNode" $ \o -> do
    requestNodeType <- o .: "tag" :: Parser String
    case requestNodeType of
      "RequestFile" -> do
        pgHeaders <- o .: "http_headers" :: Parser [PGHeader]
        id <- o .: "id"
        name <- o .: "name"
        httpUrl <- o .: "http_url"
        httpMethod <- o .: "http_method"
        httpBody <- o .: "http_body"
        let httpHeaders = (\pgHeader -> (headerKey pgHeader, headerValue pgHeader)) <$> pgHeaders
        return $ RequestNodeFromPG $ RequestFile{..}
      _ -> do
        pgChildren <- o .: "children" :: Parser [RequestNodeFromPG]
        id <- o .: "id"
        name <- o .: "name"
        let children = fromPgRequestNodeToRequestNode <$> pgChildren
        return $ RequestNodeFromPG $ RequestFolder{..}

fromPgRequestNodeToRequestNode :: RequestNodeFromPG -> RequestNode
fromPgRequestNodeToRequestNode (RequestNodeFromPG requestNode) = requestNode

instance FromField [RequestNode] where
  fromField field mdata = do
    value <- fromField field mdata :: Conversion Value
    let errorOrRequestNodes = (parseEither parseJSON) value :: Either String [RequestNode]
    either (returnError ConversionFailed field) return errorOrRequestNodes

instance FromField [RequestNodeFromPG] where
  fromField field mdata = do
    value <- fromField field mdata :: Conversion Value
    let errorOrRequestNodes = (parseEither parseJSON) value :: Either String [RequestNodeFromPG]
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
