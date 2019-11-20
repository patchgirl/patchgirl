{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Environment.App where

import           GHC.Generics
import           Data.Aeson (ToJSON(..), FromJSON(..))

-- * Model

data UpdateEnvironment
  = UpdateEnvironment deriving (Eq, Show, Generic, FromJSON)

data Environment
  = Environment deriving (Eq, Show, Generic, ToJSON)

data NewEnvironment
  = NewEnvironment deriving (Eq, Show, Generic, FromJSON)
