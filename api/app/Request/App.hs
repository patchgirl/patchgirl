{-# LANGUAGE OverloadedStrings #-}

module Request.App where

import           Data.Aeson
import           Data.Text

-- * Service

putRequests :: Item
putRequests =
  Item "test"

-- * Model

data Item =
  Item { itemName :: Text } deriving (Eq, Ord, Show)

instance ToJSON Item where
  toJSON (Item name) =
    object [ "name" .= name
           ]
