{-# LANGUAGE DeriveGeneric #-}

module Session.Model where

import           CaseInsensitive
import           Data.Aeson          (FromJSON (..), ToJSON (..),
                                      genericParseJSON, genericToJSON,
                                      parseJSON)
import           Data.Aeson.Types    (defaultOptions, fieldLabelModifier)
import           Data.Text           (Text)
import           Data.UUID
import           GHC.Generics        (Generic)
import           Servant.Auth.Server (FromJWT, ToJWT)


-- * whoami


{-
what will be sent from the server to the browser
-}

data Session
  = VisitorSession { _sessionAccountId :: UUID
                   , _sessionCsrfToken :: Text
                   }
  | SignedUserSession { _sessionAccountId       :: UUID
                      , _sessionCsrfToken       :: Text
                      , _sessionGithubEmail     :: Maybe String
                      , _sessionGithubAvatarUrl :: String
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


{-
what will be sent from the browser to the server
-}

data CookieSession
  = VisitorCookie { _cookieAccountId :: UUID
                  }
  | SignedUserCookie { _cookieAccountId       :: UUID
                     , _cookieGithubEmail     :: Maybe CaseInsensitive
                     , _cookieGithubAvatarUrl :: String
                     }
  deriving (Eq, Show, Read, Generic)

instance ToJSON CookieSession where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON CookieSession where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJWT CookieSession
instance FromJWT CookieSession
