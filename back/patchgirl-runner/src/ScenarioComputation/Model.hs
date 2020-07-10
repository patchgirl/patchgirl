{-# LANGUAGE DeriveGeneric #-}

module ScenarioComputation.Model  ( SceneInput(..)
                                  , Scene(..)
                                  , ScenarioInput(..)
                                  , SceneOutput(..)
                                  , ScenarioOutput(..)
                                  , SceneComputation(..)
                                  , ScriptException(..)
                                  ) where


import qualified Data.Aeson               as Aeson
import           Data.UUID                (UUID)
import           GHC.Generics             (Generic)

import           Interpolator
import           PgSqlComputation.Model
import           RequestComputation.Model
import           TangoScript

-- * scene input


data SceneInput
  = SceneInput { _sceneInputId         :: UUID
               , _sceneInputFileId     :: UUID
               , _sceneInputPrescript  :: TangoAst
               , _sceneInputPostscript :: TangoAst
               , _sceneInputScene      :: Scene
               }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON SceneInput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON SceneInput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * http scene input


data HttpSceneInput
  = HttpSceneInput { _httpSceneInputId         :: UUID
                   , _httpSceneInputFileId     :: UUID
                   , _httpSceneInputPrescript  :: TangoAst
                   , _httpSceneInputPostscript :: TangoAst
                   , _httpSceneInputScene      :: Scene
                   }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON HttpSceneInput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON HttpSceneInput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * pg scene input


data PgSceneInput
  = PgSceneInput { _pgSceneInputId         :: UUID
                 , _pgSceneInputFileId     :: UUID
                 , _pgSceneInputPrescript  :: TangoAst
                 , _pgSceneInputPostscript :: TangoAst
                 , _pgSceneInputScene      :: Scene
                 }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON PgSceneInput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON PgSceneInput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * scene


data Scene
  = HttpScene TemplatedRequestComputationInput
  | PgScene PgComputationInput
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON Scene where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON Scene where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * scenario input


data ScenarioInput
  = ScenarioInput { _scenarioInputId      :: UUID
                  , _scenarioInputScenes  :: [SceneInput]
                  , _scenarioInputEnvVars :: EnvironmentVars
                  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON ScenarioInput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON ScenarioInput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * scene computation


data SceneComputation
  = SceneNotRun
  | PrescriptFailed ScriptException
  | RequestFailed HttpException
  | PostscriptFailed ScriptException
  | SceneSucceeded RequestComputation
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
