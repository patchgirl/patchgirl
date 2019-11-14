{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module RequestNode.Model where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import Http
import           Data.Aeson (withObject, FromJSON(..), ToJSON, (.:), constructorTagModifier)
import           Data.Aeson.Types (Parser, camelTo2, fieldLabelModifier, genericParseJSON, defaultOptions)
import           Database.PostgreSQL.Simple.FromField hiding (name)
import           Data.Aeson (Value, parseJSON)
import Data.Aeson.Types (parseEither)

data UpdateRequestNode
  = UpdateRequestFolder { eName :: String
                        }
  | UpdateRequestFile { name :: String
                      , httpUrl :: String
                      , httpMethod :: Method
                      , httpHeaders :: [(String, String)]
                      , httpBody :: String
                      }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToField UpdateRequestNode where
  toField (UpdateRequestFolder { eName }) =
    toField (show eName)
  toField (UpdateRequestFile { name, httpUrl, httpMethod, httpBody }) =
    Many [ toField name
         , toField httpUrl
         , toField httpMethod
         , toField httpBody
         ]

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

instance FromField [RequestNode] where
  fromField field mdata = do
    value <- fromField field mdata :: Conversion Value
    let errorOrRequestNodes = (parseEither parseJSON) value :: Either String [RequestNode]
    either (returnError ConversionFailed field) return errorOrRequestNodes

newtype RequestNodeFromPG = RequestNodeFromPG RequestNode

instance FromJSON RequestNodeFromPG where
  parseJSON = withObject "RequestNode" $ \o -> do
    requestNodeType <- o .: "tag" :: Parser RequestNodeType
    case requestNodeType of
      RequestFileType -> do
        pgHeaders <- o .: "http_headers" :: Parser [PGHeader]
        let httpHeaders = (\pgHeader -> (headerKey pgHeader, headerValue pgHeader)) <$> pgHeaders
        id <- o .: "id"
        name <- o .: "name"
        httpUrl <- o .: "http_url"
        httpMethod <- o .: "http_method"
        httpBody <- o .: "http_body"
        return $ RequestNodeFromPG $ RequestFile{..}
      RequestFolderType -> do
        pgChildren <- o .: "children" :: Parser [RequestNodeFromPG]
        id <- o .: "id"
        name <- o .: "name"
        let children = fromPgRequestNodeToRequestNode <$> pgChildren
        return $ RequestNodeFromPG $ RequestFolder{..}

instance FromField [RequestNodeFromPG] where
  fromField field mdata = do
    value <- fromField field mdata :: Conversion Value
    let errorOrRequestNodes = (parseEither parseJSON) value :: Either String [RequestNodeFromPG]
    either (returnError ConversionFailed field) return errorOrRequestNodes

fromPgRequestNodeToRequestNode :: RequestNodeFromPG -> RequestNode
fromPgRequestNodeToRequestNode (RequestNodeFromPG requestNode) = requestNode

data PGHeader = PGHeader { headerKey :: String
                         , headerValue :: String
                         } deriving (Eq, Show)

instance FromJSON PGHeader where
  parseJSON = withObject "PGHeader" $ \o -> do
    headerKey <- o .: "header_key"
    headerValue <- o .: "header_value"
    return PGHeader{..}

data RequestNodeType
  = RequestFileType
  | RequestFolderType
  deriving (Eq, Show, Generic)

instance FromJSON RequestNodeType where
  parseJSON = genericParseJSON defaultOptions
    { constructorTagModifier = \s ->
        let suffixToRemove = "Type" :: String
        in take ((length s) - (length suffixToRemove)) s
    }

data ParentNodeId
  = RequestCollectionId Int
  | RequestNodeId Int
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data NewRequestFile =
  NewRequestFile { name :: String
                 , parentNodeId :: ParentNodeId
                 , httpMethod :: Method
                 } deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance ToRow NewRequestFile where
  toRow (NewRequestFile { name
                        , parentNodeId
                        , httpMethod
                        }) =
    let
      tag = "RequestFile" :: String
      noId = Nothing :: Maybe Int
    in
      case parentNodeId of
        RequestCollectionId requestCollectionId ->
          toRow ( requestCollectionId
                , noId
                , tag
                , name
                , httpMethod
                )
        RequestNodeId requestNodeId ->
          toRow ( noId
                , requestNodeId
                , tag
                , name
                , httpMethod
                )

data NewRequestFolder =
  NewRequestFolder { name :: String
                   , parentNodeId :: ParentNodeId
                   } deriving (Eq, Show, Generic)

instance ToRow NewRequestFolder where
  toRow (NewRequestFolder { name
                          , parentNodeId
                          }) =
    let
      tag = "RequestFolder" :: String
      noId = Nothing :: Maybe Int
    in
      case parentNodeId of
        RequestCollectionId requestCollectionId ->
          toRow ( requestCollectionId
                , noId
                , tag
                , name
                )
        RequestNodeId requestNodeId ->
          toRow ( noId
                , requestNodeId
                , tag
                , name
                )
