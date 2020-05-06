{-# LANGUAGE DeriveGeneric #-}

module ScenarioComputation.Model  ( SceneInput(..)
                                  , ScenarioInput(..)
                                  , SceneOutput(..)
                                  , ScenarioOutput(..)
                                  , SceneComputation(..)
                                  , ScenarioEnvironment
                                  , PrescriptException(..)
                                  ) where


import qualified Data.Aeson               as Aeson
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.UUID                (UUID)
import           GHC.Generics             (Generic)
import           RequestComputation.Model
import           TangoScript


-- * scene input


data SceneInput
  = SceneInput { _inputSceneId                      :: UUID
               , _inputSceneRequestFileNodeId       :: UUID
               , _inputScenePreScript               :: TangoAst
               , _inputSceneRequestComputationInput :: Maybe RequestComputationInput
               }
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON SceneInput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON SceneInput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * scenario input


data ScenarioInput
  = ScenarioInput { _inputScenarioId        :: UUID
                  , _inputScenarioScenes    :: [SceneInput]
                  , _inputScenarioGlobalEnv :: ScenarioEnvironment
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
  | PrescriptFailed PrescriptException
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


-- * scenario environment


type ScenarioEnvironment = Map String Expr


-- * PrescriptException


data PrescriptException
  = UnknownVariable Expr
  | AssertEqualFailed Expr Expr
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON PrescriptException where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON PrescriptException where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }
