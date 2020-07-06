{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module PgNode.Model where


import           Control.Lens                         (makeLenses)
import           Data.Aeson                           (Value)
import           Data.Aeson.Types                     (FromJSON (..), Parser,
                                                       ToJSON (..),
                                                       constructorTagModifier,
                                                       defaultOptions,
                                                       fieldLabelModifier,
                                                       genericParseJSON,
                                                       genericToJSON,
                                                       parseEither, withObject,
                                                       (.:))
import qualified Data.List                            as List
import           Data.UUID
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField hiding (name)
import           Database.PostgreSQL.Simple.ToField
import           GHC.Generics
import           Http


-- * pg node


data PgNode
  = PgFolder { _pgNodeId       :: UUID
             , _pgNodeName     :: String
             , _pgNodeChildren :: [PgNode]
             }
  | PgFile { _pgNodeId   :: UUID
           , _pgNodeName :: String
           , _pgNodeSql  :: String
           }
  deriving (Eq, Show, Generic)

$(makeLenses ''PgNode)

instance ToJSON PgNode where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON PgNode where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromField [PgNode] where
  fromField field mdata = do
    value <- fromField field mdata :: Conversion Value
    let errorOrPgNodes = parseEither parseJSON value :: Either String [PgNode]
    either (returnError ConversionFailed field) return errorOrPgNodes


-- * pg node type


data PgNodeType
  = PgFileType
  | PgFolderType
  deriving (Eq, Show, Generic)

instance FromJSON PgNodeType where
  parseJSON = genericParseJSON defaultOptions
    { constructorTagModifier = \s ->
        let suffixToRemove = "Type" :: String
        in take (length s - length suffixToRemove) s
    }


-- * pg node from pg


newtype PgNodeFromPG = PgNodeFromPG PgNode

instance FromJSON PgNodeFromPG where
  parseJSON = withObject "PgNode" $ \o -> do
    pgNodeType <- o .: "tag" :: Parser PgNodeType
    case pgNodeType of
      PgFileType -> do
        _pgNodeId <- o .: "id"
        _pgNodeName <- o .: "name"
        _pgNodeSql <- o .: "sql"
        return $ PgNodeFromPG $ PgFile{..}
      PgFolderType -> do
        _pgNodeId <- o .: "id"
        _pgNodeName <- o .: "name"
        pgChildren <- o .: "children" :: Parser [PgNodeFromPG]
        let _pgNodeChildren = fromPgPgNodeToPgNode <$> pgChildren
        return $ PgNodeFromPG $ PgFolder{..}

instance FromField [PgNodeFromPG] where
  fromField field mdata =
    (fromField field mdata :: Conversion (Maybe Value)) >>= \case
      Nothing ->
        return []
      Just value -> do
        let errorOrPgNodes = parseEither parseJSON value :: Either String [PgNodeFromPG]
        either (returnError ConversionFailed field) return errorOrPgNodes

instance FromField PgNodeFromPG where
  fromField field mdata =
    (fromField field mdata :: Conversion Value) >>= \case
      value -> do
        let errorOrPgNode = parseEither parseJSON value :: Either String PgNodeFromPG
        either (returnError ConversionFailed field) return errorOrPgNode


fromPgPgNodeToPgNode :: PgNodeFromPG -> PgNode
fromPgPgNodeToPgNode (PgNodeFromPG pgNode) = pgNode


-- * update pg node


newtype UpdatePgNode
  = UpdatePgNode { _updatePgNodeName :: String
                 }
  deriving (Eq, Show, Generic)

instance ToJSON UpdatePgNode where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON UpdatePgNode where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeLenses ''UpdatePgNode)

instance ToField UpdatePgNode where
  toField UpdatePgNode {..} =
    toField (show _updatePgNodeName)


-- * new root pg file


newtype NewRootPgFile =
  NewRootPgFile { _newRootPgFileId           :: UUID
                } deriving (Eq, Show, Generic, ToRow)

$(makeLenses ''NewRootPgFile)

instance ToJSON NewRootPgFile where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewRootPgFile where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new pg file


data NewPgFile =
  NewPgFile { _newPgFileId           :: UUID
            , _newPgFileParentNodeId :: UUID
            , _newPgFileName         :: String
            , _newPgFileSql          :: String
            } deriving (Eq, Show, Generic, ToRow)

$(makeLenses ''NewPgFile)

instance ToJSON NewPgFile where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewPgFile where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new root pg folder


data NewRootPgFolder =
  NewRootPgFolder { _newRootPgFolderId   :: UUID
                  , _newRootPgFolderName :: String
                  } deriving (Eq, Show, Generic, ToRow)

$(makeLenses ''NewRootPgFolder)

instance ToJSON NewRootPgFolder where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewRootPgFolder where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new pg folder


data NewPgFolder =
  NewPgFolder { _newPgFolderId           :: UUID
              , _newPgFolderParentNodeId :: UUID
              , _newPgFolderName         :: String
              } deriving (Eq, Show, Generic)

$(makeLenses ''NewPgFolder)

instance ToJSON NewPgFolder where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewPgFolder where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * update pg file


data UpdatePgFile
  = UpdatePgFile { _updatePgFileName :: String
                 , _updatePgFileSql  :: String
                 }
  deriving (Eq, Show, Generic)

instance ToJSON UpdatePgFile where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON UpdatePgFile where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeLenses ''UpdatePgFile)


-- * parent node id


{-
  a pg node (file or folder) can either be regular (meaning it has a folder as a parent)
  or root (meaning it is at the top of a tree hierarchy so it doesn't have a parent)
-}
data ParentNodeId
  = PgCollectionId Int
  | PGNodeId UUID
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
