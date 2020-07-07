{-# LANGUAGE DeriveGeneric #-}

module PgSqlComputation.Model where

import qualified Data.Aeson   as Aeson
import           GHC.Generics (Generic)

-- * pg computation


data PGComputation
  = PGError String
  | PGCommandOK
  | PGTuplesOk Table
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON PGComputation where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON PGComputation where
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


data Column = Column String [PGValue] deriving (Eq, Show, Read, Generic, Ord)

instance Aeson.ToJSON Column where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON Column where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * PGValue


data PGValue
  = PGString String
  | PGInt Int
  | PGBool Bool
  | PGNull
  deriving (Eq, Show, Read, Generic, Ord)

instance Aeson.ToJSON PGValue where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON PGValue where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }
