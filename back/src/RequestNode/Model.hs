{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module RequestNode.Model where


import           Control.Lens                         (makeLenses)
import           Data.Aeson                           (Value, parseJSON)
import           Data.Aeson.Types                     (FromJSON (..), Parser,
                                                       ToJSON (..),
                                                       constructorTagModifier,
                                                       defaultOptions,
                                                       fieldLabelModifier,
                                                       genericParseJSON,
                                                       genericToJSON,
                                                       parseEither, withObject,
                                                       (.:))
import           Data.UUID
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField hiding (name)
import           Database.PostgreSQL.Simple.ToField
import           GHC.Generics
import           Http


-- * request node


data RequestNode
  = RequestFolder { _requestNodeId       :: UUID
                  , _requestNodeName     :: String
                  , _requestNodeChildren :: [RequestNode]
                  }
  | RequestFile { _requestNodeId          :: UUID
                , _requestNodeName        :: String
                , _requestNodeHttpUrl     :: String
                , _requestNodeHttpMethod  :: Method
                , _requestNodeHttpHeaders :: [(String, String)]
                , _requestNodeHttpBody    :: String
                }
  deriving (Eq, Show, Generic)

$(makeLenses ''RequestNode)

instance ToJSON RequestNode where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON RequestNode where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromField [RequestNode] where
  fromField field mdata = do
    value <- fromField field mdata :: Conversion Value
    let errorOrRequestNodes = parseEither parseJSON value :: Either String [RequestNode]
    either (returnError ConversionFailed field) return errorOrRequestNodes


-- * update request node


data UpdateRequestNode
  = UpdateRequestFolder { _updateRequestNodeName :: String
                        }
  | UpdateRequestFile { _updateRequestNodeName        :: String
                      , _updateRequestNodeHttpUrl     :: String
                      , _updateRequestNodeHttpMethod  :: Method
                      , _updateRequestNodeHttpHeaders :: [(String, String)]
                      , _updateRequestNodeHttpBody    :: String
                      }
  deriving (Eq, Show, Generic)

instance ToJSON UpdateRequestNode where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON UpdateRequestNode where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeLenses ''UpdateRequestNode)

instance ToField UpdateRequestNode where
  toField UpdateRequestFolder { _updateRequestNodeName } =
    toField (show _updateRequestNodeName)
  toField UpdateRequestFile { _updateRequestNodeName
                            , _updateRequestNodeHttpUrl
                            , _updateRequestNodeHttpMethod
                            , _updateRequestNodeHttpBody
                            } =
    Many [ toField _updateRequestNodeName
         , toField _updateRequestNodeHttpUrl
         , toField _updateRequestNodeHttpMethod
         , toField _updateRequestNodeHttpBody
         ]


-- * request node from pg


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
  fromField field mdata =
    (fromField field mdata :: Conversion (Maybe Value)) >>= \case
      Nothing ->
        return []
      Just value -> do
        let errorOrRequestNodes = parseEither parseJSON value :: Either String [RequestNodeFromPG]
        either (returnError ConversionFailed field) return errorOrRequestNodes

instance FromField RequestNodeFromPG where
  fromField field mdata =
    (fromField field mdata :: Conversion Value) >>= \case
      value -> do
        let errorOrRequestNode = parseEither parseJSON value :: Either String RequestNodeFromPG
        either (returnError ConversionFailed field) return errorOrRequestNode


fromPgRequestNodeToRequestNode :: RequestNodeFromPG -> RequestNode
fromPgRequestNodeToRequestNode (RequestNodeFromPG requestNode) = requestNode


-- * pg header


data PGHeader = PGHeader { headerKey   :: String
                         , headerValue :: String
                         } deriving (Eq, Show)

instance FromJSON PGHeader where
  parseJSON = withObject "PGHeader" $ \o -> do
    headerKey <- o .: "header_key"
    headerValue <- o .: "header_value"
    return PGHeader{..}


-- * request node type


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


-- * parent node id


{-
  a request node (file or folder) can either be regular (meaning it has a folder as a parent)
  or root (meaning it is at the top of a tree hierarchy so it doesn't have a parent)
-}
data ParentNodeId
  = RequestCollectionId Int
  | RequestNodeId UUID
  deriving (Eq, Show, Generic, FromJSON, ToJSON)


-- * new request file


data NewRequestFile =
  NewRequestFile { _newRequestFileId           :: UUID
                 , _newRequestFileParentNodeId :: UUID
                 } deriving (Eq, Show, Generic, ToRow)

$(makeLenses ''NewRequestFile)

instance ToJSON NewRequestFile where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewRequestFile where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new root request file


newtype NewRootRequestFile =
  NewRootRequestFile { _newRootRequestFileId           :: UUID
                     } deriving (Eq, Show, Generic, ToRow)

$(makeLenses ''NewRootRequestFile)

instance ToJSON NewRootRequestFile where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewRootRequestFile where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new root request folder


newtype NewRootRequestFolder =
  NewRootRequestFolder { _newRootRequestFolderId           :: UUID
                       } deriving (Eq, Show, Generic, ToRow)

$(makeLenses ''NewRootRequestFolder)

instance ToJSON NewRootRequestFolder where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewRootRequestFolder where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new request folder


data NewRequestFolder =
  NewRequestFolder { _newRequestFolderId           :: UUID
                   , _newRequestFolderParentNodeId :: UUID
                   , _newRequestFolderName         :: String
                   } deriving (Eq, Show, Generic)

$(makeLenses ''NewRequestFolder)

instance ToJSON NewRequestFolder where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewRequestFolder where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }
