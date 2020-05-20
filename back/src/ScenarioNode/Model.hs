{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators          #-}

module ScenarioNode.Model where


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
import           GHC.Generics



-- * new scene


data NewScene
  = NewScene { _newSceneId                :: UUID
             , _newSceneSceneNodeParentId :: Maybe UUID
             , _newSceneRequestFileNodeId :: UUID
             }
  deriving (Eq, Show, Generic)

$(makeLenses ''NewScene)

instance ToJSON NewScene where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewScene where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * scene


data Scene
  = Scene { _sceneId                :: UUID
          , _sceneRequestFileNodeId :: UUID
          }
  deriving (Eq, Show, Generic)

$(makeLenses ''Scene)

instance ToJSON Scene where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Scene where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * scene from pg


newtype SceneFromPG = SceneFromPG Scene

instance FromJSON SceneFromPG where
  parseJSON = withObject "SceneFromPG" $ \o -> do
    _sceneId <- o .: "id"
    _sceneRequestFileNodeId <- o .: "request_node_id"
    return $ SceneFromPG $ Scene{..}

fromSceneFromPGTOScene :: SceneFromPG -> Scene
fromSceneFromPGTOScene (SceneFromPG scene) = scene


-- * scenario node


data ScenarioNode
  = ScenarioFolder { _scenarioNodeId       :: UUID
                   , _scenarioNodeName     :: String
                   , _scenarioNodeChildren :: [ScenarioNode]
                   }
  | ScenarioFile { _scenarioNodeId            :: UUID
                 , _scenarioNodeName          :: String
                 , _scenarioNodeEnvironmentId :: Maybe Int
                 , _scenarioNodeScenes        :: [Scene]
                 }
  deriving (Eq, Show, Generic)

$(makeLenses ''ScenarioNode)

instance ToJSON ScenarioNode where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON ScenarioNode where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromField [ScenarioNode] where
  fromField field mdata = do
    value <- fromField field mdata :: Conversion Value
    let errorOrScenarioNodes = parseEither parseJSON value :: Either String [ScenarioNode]
    either (returnError ConversionFailed field) return errorOrScenarioNodes


-- * scenario node type


data ScenarioNodeType
  = ScenarioFileType
  | ScenarioFolderType
  deriving (Eq, Show, Generic)

instance FromJSON ScenarioNodeType where
  parseJSON = genericParseJSON defaultOptions
    { constructorTagModifier = \s ->
        let suffixToRemove = "Type" :: String
        in take (length s - length suffixToRemove) s
    }


-- * scenario node from pg


newtype ScenarioNodeFromPG = ScenarioNodeFromPG ScenarioNode

instance FromJSON ScenarioNodeFromPG where
  parseJSON = withObject "ScenarioNode" $ \o -> do
    scenarioNodeType <- o .: "tag" :: Parser ScenarioNodeType
    case scenarioNodeType of
      ScenarioFileType -> do
        _scenarioNodeId <- o .: "id"
        _scenarioNodeName <- o .: "name"
        _scenarioNodeEnvironmentId <- o .: "environment_id"
        scenesFromPG <- o .: "scene_nodes" :: Parser [SceneFromPG]
        let _scenarioNodeScenes = map fromSceneFromPGTOScene scenesFromPG
        return $ ScenarioNodeFromPG $ ScenarioFile{..}
      ScenarioFolderType -> do
        pgChildren <- o .: "children" :: Parser [ScenarioNodeFromPG]
        _scenarioNodeId <- o .: "id"
        _scenarioNodeName <- o .: "name"
        let _scenarioNodeChildren = fromPgScenarioNodeToScenarioNode <$> pgChildren
        return $ ScenarioNodeFromPG $ ScenarioFolder{..}

instance FromField [ScenarioNodeFromPG] where
  fromField field mdata =
    (fromField field mdata :: Conversion (Maybe Value)) >>= \case
      Nothing ->
        return []
      Just value -> do
        let errorOrScenarioNodes = parseEither parseJSON value :: Either String [ScenarioNodeFromPG]
        either (returnError ConversionFailed field) return errorOrScenarioNodes

instance FromField ScenarioNodeFromPG where
  fromField field mdata =
    (fromField field mdata :: Conversion Value) >>= \case
      value -> do
        let errorOrScenarioNode = parseEither parseJSON value :: Either String ScenarioNodeFromPG
        either (returnError ConversionFailed field) return errorOrScenarioNode


fromPgScenarioNodeToScenarioNode :: ScenarioNodeFromPG -> ScenarioNode
fromPgScenarioNodeToScenarioNode (ScenarioNodeFromPG scenarioNode) = scenarioNode


-- * update scenario node


newtype UpdateScenarioNode
  = UpdateScenarioNode { _updateScenarioNodeName :: String
                      }
  deriving (Eq, Show, Generic)

instance ToJSON UpdateScenarioNode where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON UpdateScenarioNode where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeLenses ''UpdateScenarioNode)


-- * new root scenario file


data NewRootScenarioFile =
  NewRootScenarioFile { _newRootScenarioFileId            :: UUID
                      , _newRootScenarioFileName          :: String
                      , _newRootScenarioFileEnvironmentId :: Maybe Int
                      } deriving (Eq, Show, Generic, ToRow)

$(makeLenses ''NewRootScenarioFile)

instance ToJSON NewRootScenarioFile where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewRootScenarioFile where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new scenario file


data NewScenarioFile =
  NewScenarioFile { _newScenarioFileId            :: UUID
                  , _newScenarioFileName          :: String
                  , _newScenarioFileParentNodeId  :: UUID
                  , _newScenarioFileEnvironmentId :: Maybe Int
                  } deriving (Eq, Show, Generic, ToRow)

$(makeLenses ''NewScenarioFile)

instance ToJSON NewScenarioFile where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewScenarioFile where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * update scenario file


data UpdateScenarioFile =
  UpdateScenarioFile { _updateScenarioFileId            :: UUID
                     , _updateScenarioFileEnvironmentId :: Maybe Int
                     } deriving (Eq, Show, Generic, ToRow)

$(makeLenses ''UpdateScenarioFile)

instance ToJSON UpdateScenarioFile where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON UpdateScenarioFile where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new root scenario folder


newtype NewRootScenarioFolder =
  NewRootScenarioFolder { _newRootScenarioFolderId           :: UUID
                       } deriving (Eq, Show, Generic, ToRow)

$(makeLenses ''NewRootScenarioFolder)

instance ToJSON NewRootScenarioFolder where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewRootScenarioFolder where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new scenario folder


data NewScenarioFolder =
  NewScenarioFolder { _newScenarioFolderId           :: UUID
                    , _newScenarioFolderParentNodeId :: UUID
                    , _newScenarioFolderName         :: String
                    } deriving (Eq, Show, Generic)

$(makeLenses ''NewScenarioFolder)

instance ToJSON NewScenarioFolder where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewScenarioFolder where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }
