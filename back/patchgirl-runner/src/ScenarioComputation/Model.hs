{-# LANGUAGE DeriveGeneric #-}

module ScenarioComputation.Model where


import           Control.Lens             (makeLenses)
import qualified Data.Aeson               as Aeson
import           Data.UUID                (UUID)
import           GHC.Generics             (Generic)

import           Interpolator
import           PgSqlComputation.Model
import           RequestComputation.Model
import           TangoScript


-- * scenario input


data ScenarioInput
  = ScenarioInput { _scenarioInputId      :: UUID
                  , _scenarioInputScenes  :: [Scene]
                  , _scenarioInputEnvVars :: EnvironmentVars
                  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON ScenarioInput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON ScenarioInput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * scene


data Scene
  = HttpScene { _sceneId         :: UUID
              , _sceneFileId     :: UUID
              , _scenePrescript  :: TangoAst
              , _scenePostscript :: TangoAst
              , _sceneHttpInput  :: TemplatedRequestComputationInput
              }
  | PgScene { _sceneId         :: UUID
            , _sceneFileId     :: UUID
            , _scenePrescript  :: TangoAst
            , _scenePostscript :: TangoAst
            , _scenePgInput    :: PgComputationInput
            }
  deriving (Eq, Show, Generic)

$(makeLenses ''Scene)

instance Aeson.ToJSON Scene where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON Scene where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * scenario output


newtype ScenarioOutput
  = ScenarioOutput [SceneOutput]
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON ScenarioOutput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON ScenarioOutput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * scene output


data SceneOutput
  = SceneOutput { _outputSceneId                :: UUID
                , _outputSceneRequestFileNodeId :: UUID
                , _outputSceneComputation       :: SceneComputation
                }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON SceneOutput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON SceneOutput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * scene computation output


data SceneComputation
  = SceneNotRun
  | PrescriptFailed ScriptException
  | HttpSceneOk RequestComputation
  | HttpSceneFailed HttpException
  | PgSceneOk PgComputation
  | PgSceneFailed PgError
  | PgPostscriptFailed PgComputation ScriptException
  | HttpPostscriptFailed RequestComputation ScriptException
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON SceneComputation where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1
                                             , Aeson.sumEncoding = Aeson.ObjectWithSingleField
                                             }

instance Aeson.FromJSON SceneComputation where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1
                                                , Aeson.sumEncoding = Aeson.ObjectWithSingleField
                                                }


-- * scene computation


data SceneComputationOutput
  = ScenePgComputation PgComputationOutput
  | SceneHttpComputation RequestComputationOutput
  deriving (Eq, Show, Generic)


-- * succesful scene computation


data SuccesfulSceneComputation
  = SuccesfulHttpSceneComputation RequestComputation
  | SuccesfulPgSceneComputation PgComputation
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON SuccesfulSceneComputation where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON SuccesfulSceneComputation where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * script exception


data ScriptException
  = UnknownVariable Expr
  | AssertEqualFailed Expr Expr
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON ScriptException where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON ScriptException where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }
