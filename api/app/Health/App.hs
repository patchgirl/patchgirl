{-# LANGUAGE OverloadedStrings #-}

module Health.App where

import           Data.Aeson
import           Data.Text

-- * Service

getHealth :: Health
getHealth =
  Health True

-- * Model

data Health =
  Health { db :: Bool } deriving Show

instance ToJSON Health where
  toJSON (Health db) =
    object [ "db" .= (show db)
           ]
