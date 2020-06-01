{-# LANGUAGE DeriveGeneric #-}

module Interpolator ( Template(..)
                    , StringTemplate
                    , interpolate
                    , EnvironmentVars
                    , ScenarioVars
                    ) where

import qualified Data.Aeson      as Aeson
import           Data.Functor    ((<&>))
import qualified Data.List       as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe      as Maybe
import           GHC.Generics    (Generic)

import           TangoScript


-- * model

{--
scenario variables are variables defined in scripts and can only
be used in a scenario context.
They can be modified accross pre or post script scene and only
live accross a given scenario.
--}
type ScenarioVars = Map String Expr

{--
environment variables are variables defined in the environment
section. they live accross requests and scenario
--}
type EnvironmentVars = Map String StringTemplate

type StringTemplate = [Template]

data Template
  = Sentence String
  | Key String
  deriving(Eq, Show, Read, Generic, Ord)

instance Aeson.ToJSON Template where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON Template where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * interpolate request environment

interpolate
  :: EnvironmentVars
  -> ScenarioVars
  -> ScenarioVars
  -> StringTemplate
  -> String
interpolate environmentVars scenarioGlobalVars scenarioLocalVars stringTemplate =
  let
    interpolated :: [Template]
    interpolated =
      stringTemplate >>= interpolateEnvironmentVars environmentVars
      <&> interpolateScenarioVars scenarioGlobalVars
      <&> interpolateScenarioVars scenarioLocalVars
  in
    List.intercalate "" $ map templateToString interpolated

interpolateEnvironmentVars :: EnvironmentVars -> Template -> StringTemplate
interpolateEnvironmentVars environmentVars = \case
  Key key ->
    Maybe.fromMaybe [Key key] (Map.lookup key environmentVars)
  sentence -> [ sentence ]

interpolateScenarioVars :: ScenarioVars -> Template -> Template
interpolateScenarioVars scenarioVars = \case
  Key key ->
    case Map.lookup key scenarioVars >>= exprToString of
      Just str -> Sentence str
      Nothing  -> Key key
  sentence -> sentence


-- * util


templateToString :: Template -> String
templateToString = \case
  Sentence sentence -> sentence
  Key key -> "{{" ++ key ++ "}}"
