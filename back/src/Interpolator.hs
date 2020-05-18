{-# LANGUAGE DeriveGeneric #-}

module Interpolator ( Template(..)
                    , StringTemplate
                    , interpolate
                    , EnvironmentVars
                    ) where

import qualified Data.Aeson      as Aeson
import qualified Data.List       as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics    (Generic)


-- * model


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


interpolate :: EnvironmentVars -> StringTemplate -> String
interpolate environmentVars stringTemplate =
  List.intercalate "" $ map (interpolateEnvironmentVars environmentVars) stringTemplate

interpolateEnvironmentVars :: EnvironmentVars -> Template -> String
interpolateEnvironmentVars environmentVars = \case
  Key key ->
    case Map.lookup key environmentVars of
      Just value ->
        List.intercalate "" $ map templateToString value
      Nothing    ->
        templateToString (Key key)
  sentence -> templateToString sentence


-- * util


templateToString :: Template -> String
templateToString = \case
  Sentence sentence -> sentence
  Key key -> "{{" ++ key ++ "}}"
