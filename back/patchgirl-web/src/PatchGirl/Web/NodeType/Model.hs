{-# LANGUAGE DeriveGeneric          #-}

module PatchGirl.Web.NodeType.Model where

import           GHC.Generics
import           Data.Aeson.Types                     (FromJSON, parseJSON,
                                                       defaultOptions,
                                                       genericParseJSON)

data NodeType
  = File
  | Folder
  deriving (Eq, Show, Generic)

instance FromJSON NodeType where
  parseJSON = genericParseJSON defaultOptions
