{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Http where

import           Data.Aeson
import           Database.PostgreSQL.Simple.ToField
import           GHC.Generics

data Method
  = Get
  | Post
  | Put
  | Delete
  | Patch
  | Head
  | Options
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToField Method where
  toField = toField . show
