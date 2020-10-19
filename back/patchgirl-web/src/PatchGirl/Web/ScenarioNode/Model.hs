{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}

module PatchGirl.Web.ScenarioNode.Model where


import           Data.Aeson                           (Value)
import qualified Data.Aeson                           as Aeson
import           Data.Aeson.Types                     (FromJSON (..), Parser,
                                                       ToJSON (..),
                                                       defaultOptions,
                                                       fieldLabelModifier,
                                                       genericParseJSON,
                                                       genericToJSON,
                                                       parseEither, withObject,
                                                       (.:))
import qualified Data.ByteString.Char8                as B
import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as Map
import           Data.UUID
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField hiding (name)
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField   as PG
import           GHC.Generics

import           PatchGirl.Web.NodeType.Model


-- * actor type


data ActorType = HttpActor
    | PgActor
    deriving (Eq, Show, Generic)

instance PG.ToField ActorType where
  toField = PG.toField . show

instance PG.FromField ActorType where
   fromField f mdata =
     case B.unpack `fmap` mdata of
       Nothing          -> PG.returnError PG.UnexpectedNull f ""
       Just "HttpActor" -> return HttpActor
       Just "PgActor"   -> return PgActor
       _                -> PG.returnError PG.Incompatible f ""

instance ToJSON ActorType where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON ActorType where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new scene


data NewScene = NewScene
    { _newSceneId                 :: UUID
    , _newSceneSceneActorParentId :: Maybe UUID
    , _newSceneActorId            :: UUID
    , _newSceneActorType          :: ActorType
    , _newSceneVariables          :: SceneVariables
    , _newScenePrescript          :: String
    , _newScenePostscript         :: String
    }
    deriving (Eq, Show, Generic)

instance ToJSON NewScene where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewScene where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * scene actor


data SceneActor = HttpSceneActor
    { _sceneId         :: UUID
    , _sceneActorId    :: UUID
    , _sceneVariables  :: SceneVariables
    , _scenePrescript  :: String
    , _scenePostscript :: String
    }
    | PgSceneActor
    { _sceneId         :: UUID
    , _sceneActorId    :: UUID
    , _sceneVariables  :: SceneVariables
    , _scenePrescript  :: String
    , _scenePostscript :: String
    }
    deriving (Eq, Show, Generic)

instance ToJSON SceneActor where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON SceneActor where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * update scene


data UpdateScene = UpdateScene
    { _updateSceneVariables  :: SceneVariables
    , _updateScenePrescript  :: String
    , _updateScenePostscript :: String
    }
    deriving (Eq, Show, Generic)

instance ToJSON UpdateScene where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON UpdateScene where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * scene from pg


newtype SceneFromPG = SceneFromPG SceneActor

instance FromJSON SceneFromPG where
  parseJSON = withObject "SceneFromPG" $ \o -> do
    actorType <- o .: "actor_type" :: Parser ActorType
    SceneFromPG <$> case actorType of
      HttpActor -> do
        _sceneId <- o .: "id"
        _sceneActorId <- o .: "http_actor_id"
        _sceneVariables <- o .: "variables"
        _scenePrescript <- o .: "prescript"
        _scenePostscript <- o .: "postscript"
        return HttpSceneActor{..}
      PgActor -> do
        _sceneId <- o .: "id"
        _sceneActorId <- o .: "pg_actor_id"
        _sceneVariables <- o .: "variables"
        _scenePrescript <- o .: "prescript"
        _scenePostscript <- o .: "postscript"
        return PgSceneActor{..}

fromSceneFromPGTOScene :: SceneFromPG -> SceneActor
fromSceneFromPGTOScene (SceneFromPG scene) = scene


-- * scene variables


emptySceneVariable :: SceneVariables
emptySceneVariable = SceneVariables Map.empty

newtype SceneVariables = SceneVariables (Map String SceneVariableValue) deriving (Eq, Show, Generic)

instance PG.ToField SceneVariables where
  toField = PG.toField . Aeson.toJSON

instance FromField SceneVariables where
  fromField field mdata = do
    value <- fromField field mdata :: Conversion Value
    let eRes = parseEither parseJSON value :: Either String SceneVariables
    either (returnError ConversionFailed field) return eRes

instance ToJSON SceneVariables where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON SceneVariables where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- ** scene variable value


data SceneVariableValue = SceneVariableValue
    { _sceneVariableValueValue   :: String
    , _sceneVariableValueEnabled :: Bool
    }
    deriving (Eq, Show, Generic)

instance ToJSON SceneVariableValue where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON SceneVariableValue where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * scenario node


data ScenarioNode = ScenarioFolder
    { _scenarioNodeId       :: UUID
    , _scenarioNodeName     :: String
    , _scenarioNodeChildren :: [ScenarioNode]
    }
    | ScenarioFile
    { _scenarioNodeId            :: UUID
    , _scenarioNodeName          :: String
    , _scenarioNodeEnvironmentId :: Maybe UUID
    , _scenarioNodeScenes        :: [SceneActor]
    }
    deriving (Eq, Show, Generic)

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


-- * scenario node from pg


newtype ScenarioNodeFromPG = ScenarioNodeFromPG ScenarioNode

instance FromJSON ScenarioNodeFromPG where
  parseJSON = withObject "ScenarioNode" $ \o -> do
    scenarioNodeType <- o .: "tag" :: Parser NodeType
    case scenarioNodeType of
      File -> do
        _scenarioNodeId <- o .: "id"
        _scenarioNodeName <- o .: "name"
        _scenarioNodeEnvironmentId <- o .: "environment_id"
        scenesFromPG <- o .: "scene_nodes" :: Parser [SceneFromPG]
        let _scenarioNodeScenes = map fromSceneFromPGTOScene scenesFromPG
        return $ ScenarioNodeFromPG $ ScenarioFile{..}
      Folder -> do
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


-- * new root scenario file


data NewRootScenarioFile = NewRootScenarioFile
    { _newRootScenarioFileId            :: UUID
    , _newRootScenarioFileName          :: String
    , _newRootScenarioFileEnvironmentId :: Maybe UUID
    }
    deriving (Eq, Show, Generic, ToRow)

instance ToJSON NewRootScenarioFile where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewRootScenarioFile where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new scenario file


data NewScenarioFile = NewScenarioFile
    { _newScenarioFileId            :: UUID
    , _newScenarioFileName          :: String
    , _newScenarioFileParentNodeId  :: UUID
    , _newScenarioFileEnvironmentId :: Maybe UUID
    }
    deriving (Eq, Show, Generic, ToRow)

instance ToJSON NewScenarioFile where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewScenarioFile where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * update scenario file


data UpdateScenarioFile = UpdateScenarioFile
    { _updateScenarioFileId            :: UUID
    , _updateScenarioFileEnvironmentId :: Maybe UUID
    }
    deriving (Eq, Show, Generic, ToRow)

instance ToJSON UpdateScenarioFile where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON UpdateScenarioFile where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new root scenario folder


data NewRootScenarioFolder = NewRootScenarioFolder
    { _newRootScenarioFolderId   :: UUID
    , _newRootScenarioFolderName :: String
    }
    deriving (Eq, Show, Generic, ToRow)

instance ToJSON NewRootScenarioFolder where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewRootScenarioFolder where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new scenario folder


data NewScenarioFolder = NewScenarioFolder
    { _newScenarioFolderId           :: UUID
    , _newScenarioFolderParentNodeId :: UUID
    , _newScenarioFolderName         :: String
    }
    deriving (Eq, Show, Generic)

instance ToJSON NewScenarioFolder where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON NewScenarioFolder where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }
