{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module PatchGirl.Web.Id where

import           Data.Aeson.Types                     (FromJSON (..),
                                                       ToJSON (..),
                                                       defaultOptions,
                                                       genericParseJSON,
                                                       genericToJSON)
import           Data.UUID
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           GHC.Generics
import           Web.HttpApiData                      (FromHttpApiData,
                                                       ToHttpApiData)

newtype Id a = Id UUID deriving (Eq, Show, Generic)

instance ToField (Id a) where
    toField (Id uuid) = toField uuid

instance FromField (Id a) where
  fromField field mdata = do
    uuid <- fromField field mdata :: Conversion UUID
    return $ Id uuid

instance ToJSON (Id a) where
  toJSON =
    genericToJSON defaultOptions

instance FromJSON (Id a) where
  parseJSON =
    genericParseJSON defaultOptions

deriving instance FromHttpApiData (Id a)
deriving instance ToHttpApiData (Id a)

data Request = Request
data Postgres = Postgres
data Account = Account
