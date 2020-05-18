{-# LANGUAGE DeriveGeneric #-}

module Interpolator ( Template(..)
                    , StringTemplate
                    , interpolateEnvironmentVars
                    , interpolate
                    , templatedStringToString
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
        List.intercalate "" $ map templatedStringToString value
      Nothing    ->
        templatedStringToString (Key key)
  sentence -> templatedStringToString sentence


-- * util


templatedStringToString :: Template -> String
templatedStringToString = \case
  Sentence sentence -> sentence
  Key key -> "{{" ++ key ++ "}}"
