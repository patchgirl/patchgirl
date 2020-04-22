{-# LANGUAGE DeriveGeneric #-}

module ScenarioComputation.Model  ( ScenarioComputationOutput(..)
                                  , ScenarioComputationInput(..)
                                  , InputScene(..)
                                  , InputScenario(..)
                                  , OutputScene(..)
                                  , OutputScenario(..)
                                  ) where


import qualified Data.Aeson               as Aeson
import           Data.UUID                (UUID)
import           GHC.Generics             (Generic)
import           RequestComputation.Model


-- * input scene


data InputScene
  = InputScene { _inputSeneId                       :: UUID
               , _inputSceneRequestFileNodeId       :: UUID
               , _inputSceneRequestComputationInput :: RequestComputationInput
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


-- * output scene


data OutputScene
  = OutputScene { _outputSeneId                 :: UUID
                , _outputSceneRequestFileNodeId :: UUID
                , _outputSceneRequestComputationOutput :: RequestComputationResult
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
