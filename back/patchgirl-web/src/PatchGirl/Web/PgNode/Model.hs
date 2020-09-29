{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module PatchGirl.Web.PgNode.Model where


import           Data.Aeson                           (Value)
import           Data.Aeson.Types                     (FromJSON (..), Parser,
                                                       ToJSON (..),
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
import           PatchGirl.Web.NodeType.Model


-- * pg node


data PgNode = PgFolder
    { _pgNodeId       :: UUID
    , _pgNodeName     :: String
    , _pgNodeChildren :: [PgNode]
    }
    | PgFile
    { _pgNodeId       :: UUID
    , _pgNodeName     :: String
    , _pgNodeSql      :: String
    , _pgNodeHost     :: String
    , _pgNodePassword :: String
    , _pgNodePort     :: String
    , _pgNodeUser     :: String
    , _pgNodeDbName   :: String
    }
    deriving (Eq, Show, Generic)

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


-- * pg node from pg


newtype PgNodeFromPG = PgNodeFromPG PgNode

instance FromJSON PgNodeFromPG where
  parseJSON = withObject "PgNode" $ \o -> do
    pgNodeType <- o .: "tag" :: Parser NodeType
    case pgNodeType of
      File -> do
        _pgNodeId <- o .: "id"
        _pgNodeName <- o .: "name"
        _pgNodeSql <- o .: "sql"
        _pgNodeHost <- o .: "pg_host"
        _pgNodePassword <- o .: "pg_password"
        _pgNodePort <- o .: "pg_port"
        _pgNodeUser <- o .: "pg_user"
        _pgNodeDbName <- o .: "pg_dbname"
        return $ PgNodeFromPG $ PgFile{..}
      Folder -> do
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

instance ToField UpdatePgNode where
  toField UpdatePgNode {..} =
    toField (show _updatePgNodeName)


-- * new root pg file


data NewRootPgFile = NewRootPgFile
    { _newRootPgFileId       :: UUID
    , _newRootPgFileName     :: String
    , _newRootPgFileSql      :: String
    , _newRootPgFileHost     :: String
    , _newRootPgFilePassword :: String
    , _newRootPgFilePort     :: String
    , _newRootPgFileUser     :: String
    , _newRootPgFileDbName   :: String
    }
    deriving (Eq, Show, Generic, ToRow)

instance ToJSON NewRootPgFile where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewRootPgFile where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new pg file


data NewPgFile = NewPgFile
    { _newPgFileId           :: UUID
    , _newPgFileParentNodeId :: UUID
    , _newPgFileName         :: String
    , _newPgFileSql          :: String
    , _newPgFileHost         :: String
    , _newPgFilePassword     :: String
    , _newPgFilePort         :: String
    , _newPgFileUser         :: String
    , _newPgFileDbName       :: String
    }
    deriving (Eq, Show, Generic, ToRow)

instance ToJSON NewPgFile where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewPgFile where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new root pg folder


data NewRootPgFolder = NewRootPgFolder
    { _newRootPgFolderId   :: UUID
    , _newRootPgFolderName :: String
    }
    deriving (Eq, Show, Generic, ToRow)

instance ToJSON NewRootPgFolder where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewRootPgFolder where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new pg folder


data NewPgFolder = NewPgFolder
    { _newPgFolderId           :: UUID
    , _newPgFolderParentNodeId :: UUID
    , _newPgFolderName         :: String
    }
    deriving (Eq, Show, Generic)

instance ToJSON NewPgFolder where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewPgFolder where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * update pg file


data UpdatePgFile = UpdatePgFile
    { _updatePgFileName     :: String
    , _updatePgFileSql      :: String
    , _updatePgFileHost     :: String
    , _updatePgFilePassword :: String
    , _updatePgFilePort     :: String
    , _updatePgFileUser     :: String
    , _updatePgFileDbName   :: String
    }
    deriving (Eq, Show, Generic)

instance ToJSON UpdatePgFile where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON UpdatePgFile where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }
