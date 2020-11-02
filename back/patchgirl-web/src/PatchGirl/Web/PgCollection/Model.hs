{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}


module PatchGirl.Web.PgCollection.Model where

import           Data.Aeson
import qualified Database.PostgreSQL.Simple.FromRow as PG
import           GHC.Generics

import           PatchGirl.Web.Id
import           PatchGirl.Web.PgNode.Model


-- * Model


data PgCollection = PgCollection (Id PgCollection) [PgNode]
    deriving (Eq, Show, Generic, ToJSON, FromJSON, PG.FromRow)
