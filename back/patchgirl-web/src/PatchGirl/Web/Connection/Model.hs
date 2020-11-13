{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PatchGirl.Web.Connection.Model ( Connection(..)
                                      , UpdateConnection(..)
                                      , NewConnection(..)
                                      , ConnectionTag(..)
                                      ) where

import           Data.Aeson                           (FromJSON, ToJSON (..),
                                                       defaultOptions,
                                                       fieldLabelModifier,
                                                       genericToJSON, parseJSON)
import           Data.Aeson.Types                     (genericParseJSON)
import qualified Data.ByteString.Char8                as B
import qualified Database.PostgreSQL.Simple           as PG
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField   as PG
import           GHC.Generics

import           PatchGirl.Web.Id


-- * connection tag


data ConnectionTag = PgTag
    deriving (Eq, Show, Generic)

instance FromJSON ConnectionTag where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON ConnectionTag where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance PG.ToField ConnectionTag where
  toField = \case
    PgTag -> PG.toField ("PG" :: String)

instance PG.FromField ConnectionTag where
   fromField f mdata =
     case B.unpack `fmap` mdata of
       Just "PG" -> return PgTag
       _         -> PG.returnError PG.UnexpectedNull f ""


-- * connection


data Connection = Connection
    { _connectionId         :: Id Con
    , _connectionName       :: String
    , _connectionType       :: ConnectionTag
    , _connectionDbHost     :: String
    , _connectionDbPassword :: String
    , _connectionDbPort     :: String
    , _connectionDbUser     :: String
    , _connectionDbName     :: String
    }
    deriving (Eq, Show, Generic, PG.FromRow)

instance FromJSON Connection where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON Connection where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * new connection


data NewConnection = NewConnection
    { _newConnectionId         :: Id Con
    , _newConnectionName       :: String
    , _newConnectionTag        :: ConnectionTag
    , _newConnectionPgHost     :: String
    , _newConnectionPgPassword :: String
    , _newConnectionPgPort     :: String
    , _newConnectionPgUser     :: String
    , _newConnectionPgDbName   :: String
    }
    deriving (Eq, Show, Generic)

instance FromJSON NewConnection where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJSON NewConnection where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * update connection


newtype UpdateConnection
  = UpdateConnection { _updateConnectionName :: String }
  deriving (Eq, Show, Generic)

instance ToJSON UpdateConnection where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON UpdateConnection where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }
