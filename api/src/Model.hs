{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Model where

import           Data.Aeson (FromJSON(..), ToJSON(..))
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.FromField
import GHC.Generics (Generic)
import Data.CaseInsensitive (CI, original)
import Data.Text (Text, unpack)


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
