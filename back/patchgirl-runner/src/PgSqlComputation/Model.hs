{-# LANGUAGE DeriveGeneric #-}

module PgSqlComputation.Model where

import qualified Data.Aeson   as Aeson
import           GHC.Generics (Generic)

-- * pg computation


data PgComputation
  = PgError String
  | PgCommandOK
  | PgTuplesOk Table
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON PgComputation where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON PgComputation where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * table


newtype Table = Table [Column] deriving (Eq, Show, Read, Generic, Ord)

instance Aeson.ToJSON Table where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON Table where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * column


data Column = Column String [PgValue] deriving (Eq, Show, Read, Generic, Ord)

instance Aeson.ToJSON Column where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON Column where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * PGValue


data PgValue
  = PgString String
  | PgInt Int
  | PgBool Bool
  | PgNull
  deriving (Eq, Show, Read, Generic, Ord)

instance Aeson.ToJSON PgValue where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON PgValue where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }
