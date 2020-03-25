{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Account.Model where

import           Data.Aeson                       (FromJSON, ToJSON (..),
                                                   genericParseJSON,
                                                   genericToJSON, parseJSON)
import           Data.Aeson.Types                 (defaultOptions,
                                                   fieldLabelModifier)
import           Data.UUID
import           Database.PostgreSQL.Simple       (FromRow)
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics                     (Generic)
import           Model
