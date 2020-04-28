{-# LANGUAGE DeriveGeneric #-}

module ScenarioComputation.Model  ( ScenarioComputationOutput(..)
                                  , ScenarioComputationInput(..)
                                  , InputScene(..)
                                  , InputScenario(..)
                                  , OutputScene(..)
                                  , OutputScenario(..)
                                  , SceneComputation(..)
                                  ) where


import qualified Data.Aeson               as Aeson
import           Data.UUID                (UUID)
import           GHC.Generics             (Generic)
import           RequestComputation.Model


-- * input scene


data InputScene
  = InputScene { _inputSceneId                      :: UUID
               , _inputSceneRequestFileNodeId       :: UUID
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
  = InputScenario { _inputScenarioId     :: UUID
                  , _inputScenarioScenes :: [InputScene]
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
  = SceneRun RequestComputationResult
  | SceneNotRun
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
  = OutputScene { _outputSceneId                       :: UUID
                , _outputSceneRequestFileNodeId        :: UUID
                , _outputSceneRequestComputationOutput :: SceneComputation
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
