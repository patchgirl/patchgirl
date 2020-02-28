{-# LANGUAGE DeriveGeneric #-}

module Session.Model where

import           Data.Aeson          (FromJSON (..), ToJSON (..),
                                      genericParseJSON, genericToJSON,
                                      parseJSON)
import           Data.Aeson.Types    (defaultOptions, fieldLabelModifier)
import           Data.Text           (Text)
import           Data.UUID
import           GHC.Generics        (Generic)
import           Model
import           Servant.Auth.Server (FromJWT, ToJWT)


-- * login


data SignIn
  = SignIn { _signInEmail    :: CaseInsensitive
           , _signInPassword :: String
           }
  deriving (Eq, Show, Read, Generic)

instance ToJSON SignIn where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON SignIn where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }


-- * whoami


data Session
  = VisitorSession { _sessionAccountId :: UUID
                   , _sessionCsrfToken :: Text
                   }
  | SignedUserSession { _sessionAccountId :: UUID
                      , _sessionCsrfToken :: Text
                      , _sessionEmail     :: String
                      }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Session where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJWT Session
instance FromJSON Session where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJWT Session


-- * session


data CookieSession
  = VisitorCookie { _cookieAccountId :: UUID
                  }
  | SignedUserCookie { _cookieAccountId    :: UUID
                     , _cookieAccountEmail :: CaseInsensitive
                     }
  deriving (Eq, Show, Read, Generic)

instance ToJSON CookieSession where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJWT CookieSession
instance FromJSON CookieSession where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJWT CookieSession


-- * sign up


newtype SignUp
  = SignUp { _signUpEmail :: CaseInsensitive }
  deriving (Eq, Show, Read, Generic)

instance ToJSON SignUp where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON SignUp where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }
