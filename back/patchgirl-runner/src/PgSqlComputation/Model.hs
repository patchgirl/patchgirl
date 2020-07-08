{-# LANGUAGE DeriveGeneric #-}

module PgSqlComputation.Model where

import qualified Data.Aeson   as Aeson
import           GHC.Generics (Generic)

import           Interpolator

-- * pg computation input

data PgComputationInput
  = PgComputationInput { _pgComputationInputSql             :: StringTemplate
                       , _pgComputationInputEnvironmentVars :: EnvironmentVars
                       , _pgComputationInputPgConnection    :: PgConnection StringTemplate
                       } deriving (Eq, Show, Generic)

instance Aeson.ToJSON PgComputationInput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON PgComputationInput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


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


-- * PgValue


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


-- * pg connection


data PgConnection a = PgConnection { _pgConnectionHost     :: a
                                   , _pgConnectionPort     :: a
                                   , _pgConnectionUser     :: a
                                   , _pgConnectionPassword :: a
                                   , _pgConnectionDbName   :: a
                                   } deriving (Eq, Show, Generic)

instance Aeson.ToJSON (PgConnection StringTemplate) where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON (PgConnection StringTemplate) where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }
