{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Model where

import           Data.Aeson                           (FromJSON (..),
                                                       ToJSON (..))
import           Data.CaseInsensitive                 (CI, original)
import           Data.Text                            (Text, unpack)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import           GHC.Generics                         (Generic)

data CaseInsensitive
  = CaseInsensitive String
  deriving (Eq, Show, Read, Generic)

instance ToJSON CaseInsensitive
instance FromJSON CaseInsensitive

instance ToField CaseInsensitive where
    toField (CaseInsensitive s) = toField s

instance FromField CaseInsensitive where
  fromField field mdata = do
    foo <- fromField field mdata :: Conversion (CI Text)
    return $ CaseInsensitive $ (unpack . original) foo
