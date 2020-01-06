{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Http where

import           Data.Aeson
import           Database.PostgreSQL.Simple.ToField
import           GHC.Generics


-- * method


data Method
  = Get
  | Post
  | Put
  | Delete
  | Patch
  | Head
  | Options
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

methodToString :: Method -> String
methodToString = \case
  Get -> "GET"
  Post -> "POST"
  Put -> "PUT"
  Delete -> "DELETE"
  Patch -> "PATCH"
  Head -> "HEAD"
  Options -> "OPTIONS"


instance ToField Method where
  toField = toField . show


-- * scheme


data Scheme
  = Http
  | Https
  deriving (Eq, Show, Read, Generic)

instance ToJSON Scheme
instance FromJSON Scheme
