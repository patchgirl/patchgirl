{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}


module PatchGirl.Web.ScenarioCollection.Model where


import           Data.Aeson
import qualified Database.PostgreSQL.Simple.FromRow as PG
import           GHC.Generics

import           PatchGirl.Web.Id
import           PatchGirl.Web.ScenarioNode.Model


-- * Model


data ScenarioCollection = ScenarioCollection (Id ScenarioCol) [ScenarioNode]
    deriving (Eq, Show, Generic, ToJSON, FromJSON, PG.FromRow)
