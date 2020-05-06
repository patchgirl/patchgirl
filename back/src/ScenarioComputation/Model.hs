{-# LANGUAGE DeriveGeneric #-}

module ScenarioComputation.Model  ( ScenarioComputationOutput(..)
                                  , ScenarioComputationInput(..)
                                  , InputScene(..)
                                  , InputScenario(..)
                                  , OutputScene(..)
                                  , OutputScenario(..)
                                  , SceneComputation(..)
                                  , ScenarioEnvironment
                                  , PrescriptOutput(..)
                                  ) where


import qualified Data.Aeson               as Aeson
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.UUID                (UUID)
import           GHC.Generics             (Generic)
import           RequestComputation.Model
import           TangoScript


-- * input scene


data InputScene
  = InputScene { _inputSceneId                      :: UUID
               , _inputSceneRequestFileNodeId       :: UUID
               , _inputScenePreScript               :: TangoAst
               , _inputSceneRequestComputationInput :: Maybe RequestComputationInput
               }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON InputScene where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON InputScene where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * scenario input


data InputScenario
  = InputScenario { _inputScenarioId        :: UUID
                  , _inputScenarioScenes    :: [InputScene]
                  , _inputScenarioGlobalEnv :: ScenarioEnvironment
                  }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON InputScenario where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON InputScenario where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * scene computation


data SceneComputation
  = SceneNotRun
  | PrescriptFailed
  | RequestFailed HttpException
  | PostscriptFailed RequestComputationOutput
  | SceneSucceeded RequestComputationOutput
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


-- * output scene


data OutputScene
  = OutputScene { _outputSceneId                :: UUID
                , _outputSceneRequestFileNodeId :: UUID
                , _outputSceneComputation       :: SceneComputation
                }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON OutputScene where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON OutputScene where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * scenario output


data OutputScenario
  = OutputScenario { _outputScenarioId     :: UUID
                   , _outputScenarioScenes :: [OutputScene]
                   }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON OutputScenario where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON OutputScenario where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * scenario computation input


newtype ScenarioComputationInput
  = ScenarioComputationInput InputScenario
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON ScenarioComputationInput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON ScenarioComputationInput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * scenario computation output


newtype ScenarioComputationOutput
  = ScenarioComputationOutput OutputScenario
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON ScenarioComputationOutput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON ScenarioComputationOutput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

-- * scenario environment


type ScenarioEnvironment = Map String Expr


-- * prescript


data PrescriptOutput
  = PrescriptOutput { _outputPrescriptNewGlobalEnvironment :: ScenarioEnvironment
                    , _outputPrescriptNewLocalEnvironment :: ScenarioEnvironment
                    }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON PrescriptOutput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON PrescriptOutput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }
