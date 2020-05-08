{-# LANGUAGE DeriveGeneric #-}

module Interpolator ( TemplatedString(..)
                    , interpolateRequestEnvironment
                    , templatedStringToString
                    ) where

import qualified Data.Aeson        as Aeson
import qualified Data.Map.Strict   as Map
import           GHC.Generics      (Generic)

import           Environment.Model


-- * model


data TemplatedString
  = Sentence String
  | Key String
  deriving(Eq, Show, Read, Generic, Ord)

instance Aeson.ToJSON TemplatedString where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON TemplatedString where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * interpolate request environment


interpolateRequestEnvironment :: RequestEnvironment -> TemplatedString -> TemplatedString
interpolateRequestEnvironment requestEnvironment = \case
  Key key ->
    case Map.lookup key requestEnvironment of
      Just value -> Sentence value
      Nothing    -> Key key
  sentence -> sentence


-- * util


templatedStringToString :: TemplatedString -> String
templatedStringToString = \case
  Sentence sentence -> sentence
  Key key -> "{{" ++ key ++ "}}"
