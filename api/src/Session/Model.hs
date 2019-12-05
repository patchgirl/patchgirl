{-# LANGUAGE DeriveGeneric #-}

module Session.Model where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant.Auth.Server (ToJWT, FromJWT)
import Model


data Session =
  Session { accountId :: Int
          , accountEmail :: CaseInsensitive
          } deriving (Eq, Show, Read, Generic)

instance ToJSON Session
instance ToJWT Session
instance FromJSON Session
instance FromJWT Session
