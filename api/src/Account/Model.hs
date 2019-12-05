{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Account.Model where

import           Data.Aeson (ToJSON(..), genericToJSON)
import           Data.Aeson.Types (fieldLabelModifier, defaultOptions)
import  Model
import GHC.Generics (Generic)
import           Database.PostgreSQL.Simple (FromRow)


-- * model


data Account =
  Account { _accountId :: Int
          , _accountEmail :: CaseInsensitive
          }
  deriving (Eq, Show, Generic, FromRow)

instance ToJSON Account where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }
