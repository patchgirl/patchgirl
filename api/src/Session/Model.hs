{-# LANGUAGE DeriveGeneric #-}

module Session.Model where

import Data.Aeson (FromJSON, ToJSON)
import           Data.Aeson (ToJSON(..), genericToJSON)
import           Data.Aeson.Types (fieldLabelModifier, defaultOptions)
import GHC.Generics (Generic)
import Servant.Auth.Server (ToJWT, FromJWT)
import Model

-- * login

data Login
  = Login { email :: CaseInsensitive
          , password :: String
          }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

-- * session

data Session
  = Visitor { _sessionId :: Int }
  | SignedUser { _sessionId :: Int
               , _sessionEmail :: CaseInsensitive
               }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Session where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJWT Session
instance FromJSON Session
instance FromJWT Session
