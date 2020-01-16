{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Account.Model where

import           Data.Aeson                       (FromJSON, ToJSON (..),
                                                   genericParseJSON,
                                                   genericToJSON, parseJSON)
import           Data.Aeson.Types                 (defaultOptions,
                                                   fieldLabelModifier)
import           Database.PostgreSQL.Simple       (FromRow)
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics                     (Generic)
import           Model


-- * account


data Account =
  Account { _accountId    :: Int
          , _accountEmail :: CaseInsensitive
          }
  deriving (Eq, Show, Generic, FromRow)

instance ToJSON Account where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new account


data NewAccount =
  NewAccount { _newAccountEmail :: CaseInsensitive
             }
  deriving (Eq, Show, Generic, FromRow, ToRow)


-- * created account


data CreatedAccount =
  CreatedAccount { _accountCreatedId          :: Int
                 , _accountCreatedEmail       :: CaseInsensitive
                 , _accountCreatedSignUpToken :: String
                 }
  deriving (Eq, Show, Generic, FromRow)


-- * initialize password


data InitializePassword =
  InitializePassword { _initializePasswordAccountId :: Int
                     , _initializePasswordEmail     :: CaseInsensitive
                     , _initializePasswordPassword  :: String
                     , _initializePasswordToken     :: String
                     }
  deriving (Eq, Show, Generic, FromRow)

instance FromJSON InitializePassword where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON InitializePassword where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }
