{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PatchGirl.Web.Environment.Model ( KeyValue(..)
                         , PGEnvironmentWithKeyValue(..)
                         , PGEnvironmentWithoutKeyValue(..)
                         , NewKeyValue(..)
                         , Environment(..)
                         , UpdateEnvironment(..)
                         , NewEnvironment(..)
                         ) where

import           Data.Aeson                 (FromJSON, ToJSON (..),
                                             defaultOptions, fieldLabelModifier,
                                             genericToJSON, parseJSON)
import           Data.Aeson.Types           (genericParseJSON)
import           Data.UUID                  (UUID)
import qualified Database.PostgreSQL.Simple as PG
import           GHC.Generics

import           PatchGirl.Web.Id


-- * key value


data KeyValue = KeyValue
    { _keyValueId     :: UUID
    , _keyValueKey    :: String
    , _keyValueValue  :: String
    , _keyValueHidden :: Bool
    }
    deriving (Eq, Show, Generic, PG.FromRow)

instance FromJSON KeyValue where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON KeyValue where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * environment


data Environment = Environment
    { _environmentId        :: Id EnvId
    , _environmentName      :: String
    , _environmentKeyValues :: [KeyValue]
    }
    deriving (Eq, Show, Generic)

instance FromJSON Environment where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON Environment where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * get environments


data PGEnvironmentWithKeyValue = PGEnvironmentWithKeyValue
    { _pgEnvironmentWithKeyValueEnvironmentId   :: Id EnvId
    , _pgEnvironmentWithKeyValueEnvironmentName :: String
    , _pgEnvironmentWithKeyValueKeyValueId      :: UUID
    , _pgEnvironmentWithKeyValueKey             :: String
    , _pgEnvironmentWithKeyValueValue           :: String
    , _pgEnvironmentWithKeyValueHidden          :: Bool
    }
    deriving (Generic, PG.FromRow)

data PGEnvironmentWithoutKeyValue = PGEnvironmentWithoutKeyValue
    { _pgEnvironmentWithoutKeyValueEnvironmentId   :: Id EnvId
    , _pgEnvironmentWithoutKeyValueEnvironmentName :: String
    }
    deriving (Generic, PG.FromRow)


-- * new environment


data NewEnvironment = NewEnvironment
    { _newEnvironmentId   :: Id EnvId
    , _newEnvironmentName :: String
    }
    deriving (Eq, Show, Generic)

instance FromJSON NewEnvironment where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON NewEnvironment where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * update environment


newtype UpdateEnvironment
  = UpdateEnvironment { _updateEnvironmentName :: String }
  deriving (Eq, Show, Generic)

instance ToJSON UpdateEnvironment where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON UpdateEnvironment where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * upsert key values


data NewKeyValue = NewKeyValue
    { _newKeyValueId     :: UUID
    , _newKeyValueKey    :: String
    , _newKeyValueValue  :: String
    , _newKeyValueHidden :: Bool
    }
    deriving (Eq, Show, Generic)

instance FromJSON NewKeyValue where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON NewKeyValue where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }
