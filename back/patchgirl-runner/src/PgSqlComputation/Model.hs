{-# LANGUAGE DeriveGeneric #-}

module PgSqlComputation.Model where

import qualified Data.Aeson   as Aeson
import           GHC.Generics (Generic)

import           Interpolator


-- * pg computation input


data PgComputationInput
  = PgComputationInput { _pgComputationInputSql             :: StringTemplate
                       , _pgComputationInputPgConnection    :: TemplatedPgConnection
                       } deriving (Eq, Show, Generic)

instance Aeson.ToJSON PgComputationInput where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON PgComputationInput where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * templated pg connection


data TemplatedPgConnection = TemplatedPgConnection { _templatedPgConnectionHost     :: StringTemplate
                                                   , _templatedPgConnectionPort     :: StringTemplate
                                                   , _templatedPgConnectionUser     :: StringTemplate
                                                   , _templatedPgConnectionPassword :: StringTemplate
                                                   , _templatedPgConnectionDbName   :: StringTemplate
                                                   } deriving (Eq, Show, Generic)

instance Aeson.ToJSON TemplatedPgConnection where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON TemplatedPgConnection where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * pg computation output


type PgComputationOutput
  = Either PgError PgComputation


-- * pg error


newtype PgError = PgError String deriving (Eq, Show, Generic)


instance Aeson.ToJSON PgError where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON PgError where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * pg computation


data PgComputation
  = PgCommandOK
  | PgTuplesOk [Row]
  deriving (Eq, Show, Generic)

instance Aeson.ToJSON PgComputation where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1
                                             , Aeson.sumEncoding = Aeson.ObjectWithSingleField
                                             }

instance Aeson.FromJSON PgComputation where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1
                                                , Aeson.sumEncoding = Aeson.ObjectWithSingleField
                                                }


-- * row


newtype Row = Row [ (String, PgValue) ] deriving (Eq, Show, Read, Generic, Ord)

instance Aeson.ToJSON Row where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON Row where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

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
  | PgFloat Float
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


data PgConnection = PgConnection { _pgConnectionHost     :: String
                                 , _pgConnectionPort     :: String
                                 , _pgConnectionUser     :: String
                                 , _pgConnectionPassword :: String
                                 , _pgConnectionDbName   :: String
                                 } deriving (Eq, Show)
