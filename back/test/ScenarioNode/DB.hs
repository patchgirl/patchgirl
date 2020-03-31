{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}

module ScenarioNode.DB where

import qualified Data.ByteString                      as BS
import qualified Data.ByteString.UTF8                 as BSU
import qualified Data.Maybe                           as Maybe
import qualified Data.Strings                         as Strings
import           Data.UUID
import           Database.PostgreSQL.Simple
import qualified Database.PostgreSQL.Simple.FromField as PG
import           Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple.Types     as PG
import           GHC.Generics

import           Http
import           ScenarioNode.Model


-- * select fake scenario folder
