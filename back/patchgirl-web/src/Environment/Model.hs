{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Environment.Model ( KeyValue(..)
                         , keyValueId
                         , keyValueKey
                         , keyValueValue
                         , keyValueHidden
                         , PGEnvironmentWithKeyValue(..)
                         , pgEnvironmentWithKeyValueEnvironmentId
                         , pgEnvironmentWithKeyValueEnvironmentName
                         , pgEnvironmentWithKeyValueKeyValueId
                         , pgEnvironmentWithKeyValueKey
                         , pgEnvironmentWithKeyValueValue
                         , pgEnvironmentWithKeyValueHidden
                         , PGEnvironmentWithoutKeyValue(..)
                         , pgEnvironmentWithoutKeyValueEnvironmentId
                         , pgEnvironmentWithoutKeyValueEnvironmentName
                         , NewKeyValue(..)
                         , newKeyValueKey
                         , newKeyValueValue
                         , newKeyValueHidden
                         , Environment(..)
                         , environmentId
                         , environmentName
                         , environmentKeyValues
                         , UpdateEnvironment(..)
                         , updateEnvironmentName
                         , NewEnvironment(..)
                         , newEnvironmentName
                         ) where

import           Control.Lens               (makeLenses)
import           Data.Aeson                 (FromJSON, ToJSON (..),
                                             defaultOptions, fieldLabelModifier,
                                             genericToJSON, parseJSON)
import           Data.Aeson.Types           (genericParseJSON)
import qualified Database.PostgreSQL.Simple as PG
import           GHC.Generics


-- * key value


data KeyValue =
  KeyValue { _keyValueId    :: Int
           , _keyValueKey   :: String
           , _keyValueValue :: String
           , _keyValueHidden :: Bool
           } deriving (Eq, Show, Generic, PG.FromRow)

instance FromJSON KeyValue where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON KeyValue where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeLenses ''KeyValue)


-- * environment


data Environment
  = Environment { _environmentId        :: Int
                , _environmentName      :: String
                , _environmentKeyValues :: [KeyValue]
                } deriving (Eq, Show, Generic)

instance FromJSON Environment where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON Environment where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeLenses ''Environment)


-- * get environments


data PGEnvironmentWithKeyValue =
  PGEnvironmentWithKeyValue { _pgEnvironmentWithKeyValueEnvironmentId   :: Int
                            , _pgEnvironmentWithKeyValueEnvironmentName :: String
                            , _pgEnvironmentWithKeyValueKeyValueId      :: Int
                            , _pgEnvironmentWithKeyValueKey             :: String
                            , _pgEnvironmentWithKeyValueValue           :: String
                            , _pgEnvironmentWithKeyValueHidden          :: Bool
                            } deriving (Generic, PG.FromRow)

data PGEnvironmentWithoutKeyValue =
  PGEnvironmentWithoutKeyValue { _pgEnvironmentWithoutKeyValueEnvironmentId   :: Int
                               , _pgEnvironmentWithoutKeyValueEnvironmentName :: String
                               } deriving (Generic, PG.FromRow)


$(makeLenses ''PGEnvironmentWithKeyValue)
$(makeLenses ''PGEnvironmentWithoutKeyValue)


-- * new environment


newtype NewEnvironment
  = NewEnvironment { _newEnvironmentName :: String
                   } deriving (Eq, Show, Generic)

instance FromJSON NewEnvironment where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON NewEnvironment where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeLenses ''NewEnvironment)


-- * update environment


newtype UpdateEnvironment
  = UpdateEnvironment { _updateEnvironmentName :: String }
  deriving (Eq, Show, Generic)

instance ToJSON UpdateEnvironment where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeLenses ''UpdateEnvironment)

instance FromJSON UpdateEnvironment where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * upsert key values


data NewKeyValue
  = NewKeyValue { _newKeyValueKey   :: String
                , _newKeyValueValue :: String
                , _newKeyValueHidden  :: Bool
                }
  deriving (Eq, Show, Generic)

instance FromJSON NewKeyValue where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON NewKeyValue where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

$(makeLenses ''NewKeyValue)


-- * scenario vars
