{-# LANGUAGE DeriveGeneric #-}

module Session.Model where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Aeson          (ToJSON (..), genericToJSON)
import           Data.Aeson.Types    (defaultOptions, fieldLabelModifier)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Model
import           Servant.Auth.Server (FromJWT, ToJWT)

-- * login


data Login
  = Login { email    :: CaseInsensitive
          , password :: String
          }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login


-- * whoami

data Session
  = VisitorSession { _sessionAccountId :: Int
                   , _sessionCsrfToken :: Text
                   }
  | SignedUserSession { _sessionAccountId :: Int
                      , _sessionCsrfToken :: Text
                      , _sessionEmail     :: String
                      }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Session where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJWT Session
instance FromJSON Session
instance FromJWT Session


-- * session


data CookieSession
  = VisitorCookie { _cookieAccountId :: Int
                  }
  | SignedUserCookie { _cookieAccountId    :: Int
                     , _cookieAccountEmail :: CaseInsensitive
                     }
  deriving (Eq, Show, Read, Generic)

instance ToJSON CookieSession where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJWT CookieSession
instance FromJSON CookieSession
instance FromJWT CookieSession
