{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module RequestNode.Model where

import           Control.Lens                         (makeFieldsNoPrefix)
import           Data.Aeson                           (Value, parseJSON)
import           Data.Aeson.Types                     (FromJSON (..), Parser,
                                                       ToJSON (..), camelTo2,
                                                       constructorTagModifier,
                                                       defaultOptions,
                                                       fieldLabelModifier,
                                                       genericParseJSON,
                                                       genericToJSON,
                                                       parseEither, withObject,
                                                       (.:))
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField hiding (name)
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import           Http

data UpdateRequestNode
  = UpdateRequestFolder { _name :: String
                        }
  | UpdateRequestFile { _name        :: String
                      , _httpUrl     :: String
                      , _httpMethod  :: Method
                      , _httpHeaders :: [(String, String)]
                      , _httpBody    :: String
                      }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON UpdateRequestNode where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeFieldsNoPrefix ''UpdateRequestNode)

instance ToField UpdateRequestNode where
  toField UpdateRequestFolder { _name } =
    toField (show _name)
  toField UpdateRequestFile { _name, _httpUrl, _httpMethod, _httpBody } =
    Many [ toField _name
         , toField _httpUrl
         , toField _httpMethod
         , toField _httpBody
         ]

data RequestNode
  = RequestFolder { _requestNodeId       :: Int
                  , _requestNodeName     :: String
                  , _requestNodeChildren :: [RequestNode]
                  }
  | RequestFile { _requestNodeId          :: Int
                , _requestNodeName        :: String
                , _requestNodeHttpUrl     :: String
                , _requestNodeHttpMethod  :: Method
                , _requestNodeHttpHeaders :: [(String, String)]
                , _requestNodeHttpBody    :: String
                }
  deriving (Eq, Show, Generic)

instance ToJSON RequestNode where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON RequestNode where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromField [RequestNode] where
  fromField field mdata = do
    value <- fromField field mdata :: Conversion Value
    let errorOrRequestNodes = parseEither parseJSON value :: Either String [RequestNode]
    either (returnError ConversionFailed field) return errorOrRequestNodes

newtype RequestNodeFromPG = RequestNodeFromPG RequestNode

instance FromJSON RequestNodeFromPG where
  parseJSON = withObject "RequestNode" $ \o -> do
    requestNodeType <- o .: "tag" :: Parser RequestNodeType
    case requestNodeType of
      RequestFileType -> do
        pgHeaders <- o .: "http_headers" :: Parser [PGHeader]
        let _requestNodeHttpHeaders = (\pgHeader -> (headerKey pgHeader, headerValue pgHeader)) <$> pgHeaders
        _requestNodeId <- o .: "id"
        _requestNodeName <- o .: "name"
        _requestNodeHttpUrl <- o .: "http_url"
        _requestNodeHttpMethod <- o .: "http_method"
        _requestNodeHttpBody <- o .: "http_body"
        return $ RequestNodeFromPG $ RequestFile{..}
      RequestFolderType -> do
        pgChildren <- o .: "children" :: Parser [RequestNodeFromPG]
        _requestNodeId <- o .: "id"
        _requestNodeName <- o .: "name"
        let _requestNodeChildren = fromPgRequestNodeToRequestNode <$> pgChildren
        return $ RequestNodeFromPG $ RequestFolder{..}

instance FromField [RequestNodeFromPG] where
  fromField field mdata = do
    value <- fromField field mdata :: Conversion Value
    let errorOrRequestNodes = parseEither parseJSON value :: Either String [RequestNodeFromPG]
    either (returnError ConversionFailed field) return errorOrRequestNodes

fromPgRequestNodeToRequestNode :: RequestNodeFromPG -> RequestNode
fromPgRequestNodeToRequestNode (RequestNodeFromPG requestNode) = requestNode

data PGHeader = PGHeader { headerKey   :: String
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
        in take (length s - length suffixToRemove) s
    }

data ParentNodeId
  = RequestCollectionId Int
  | RequestNodeId Int
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data NewRequestFile =
  NewRequestFile { _name         :: String
                 , _parentNodeId :: ParentNodeId
                 , _httpMethod   :: Method
                 } deriving (Eq, Show, Generic, FromJSON)

instance ToJSON NewRequestFile where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToRow NewRequestFile where
  toRow NewRequestFile { _name
                       , _parentNodeId
                       , _httpMethod
                       } =
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
  NewRequestFolder { _name         :: String
                   , _parentNodeId :: ParentNodeId
                   } deriving (Eq, Show, Generic)

instance ToRow NewRequestFolder where
  toRow NewRequestFolder { _name
                         , _parentNodeId
                         } =
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
