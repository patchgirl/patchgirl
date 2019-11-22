{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}

module RequestNode.Model where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import Http
import           Data.Aeson (withObject, FromJSON(..), ToJSON(..), (.:), constructorTagModifier, genericToJSON)
import           Data.Aeson.Types (Parser, camelTo2, fieldLabelModifier, genericParseJSON, defaultOptions)
import           Database.PostgreSQL.Simple.FromField hiding (name)
import           Data.Aeson (Value, parseJSON)
import Data.Aeson.Types (parseEither)
import Control.Lens (makeFieldsNoPrefix)


data UpdateRequestNode
  = UpdateRequestFolder { _name :: String
                        }
  | UpdateRequestFile { _name :: String
                      , _httpUrl :: String
                      , _httpMethod :: Method
                      , _httpHeaders :: [(String, String)]
                      , _httpBody :: String
                      }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON UpdateRequestNode where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeFieldsNoPrefix ''UpdateRequestNode)

instance ToField UpdateRequestNode where
  toField (UpdateRequestFolder { _name }) =
    toField (show _name)
  toField (UpdateRequestFile { _name, _httpUrl, _httpMethod, _httpBody }) =
    Many [ toField _name
         , toField _httpUrl
         , toField _httpMethod
         , toField _httpBody
         ]

data RequestNode
  = RequestFolder { _id :: Int
                  , _name :: String
                  , _children :: [RequestNode]
                  }
  | RequestFile { _id :: Int
                , _name :: String
                , _httpUrl :: String
                , _httpMethod :: Method
                , _httpHeaders :: [(String, String)]
                , _httpBody :: String
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
        let _httpHeaders = (\pgHeader -> (headerKey pgHeader, headerValue pgHeader)) <$> pgHeaders
        _id <- o .: "id"
        _name <- o .: "name"
        _httpUrl <- o .: "http_url"
        _httpMethod <- o .: "http_method"
        _httpBody <- o .: "http_body"
        return $ RequestNodeFromPG $ RequestFile{..}
      RequestFolderType -> do
        pgChildren <- o .: "children" :: Parser [RequestNodeFromPG]
        _id <- o .: "id"
        _name <- o .: "name"
        let _children = fromPgRequestNodeToRequestNode <$> pgChildren
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
  NewRequestFile { _name :: String
                 , _parentNodeId :: ParentNodeId
                 , _httpMethod :: Method
                 } deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance ToRow NewRequestFile where
  toRow (NewRequestFile { _name
                        , _parentNodeId
                        , _httpMethod
                        }) =
    let
      tag = "RequestFile" :: String
      noId = Nothing :: Maybe Int
    in
      case _parentNodeId of
        RequestCollectionId requestCollectionId ->
          toRow ( requestCollectionId
                , noId
                , tag
                , _name
                , _httpMethod
                )
        RequestNodeId requestNodeId ->
          toRow ( noId
                , requestNodeId
                , tag
                , _name
                , _httpMethod
                )

data NewRequestFolder =
  NewRequestFolder { _name :: String
                   , _parentNodeId :: ParentNodeId
                   } deriving (Eq, Show, Generic)

instance ToRow NewRequestFolder where
  toRow (NewRequestFolder { _name
                          , _parentNodeId
                          }) =
    let
      tag = "RequestFolder" :: String
      noId = Nothing :: Maybe Int
    in
      case _parentNodeId of
        RequestCollectionId requestCollectionId ->
          toRow ( requestCollectionId
                , noId
                , tag
                , _name
                )
        RequestNodeId requestNodeId ->
          toRow ( noId
                , requestNodeId
                , tag
                , _name
                )
