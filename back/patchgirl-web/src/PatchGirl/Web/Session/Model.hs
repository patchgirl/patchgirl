{-# LANGUAGE DeriveGeneric #-}

module PatchGirl.Web.Session.Model where

import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                genericParseJSON, genericToJSON,
                                                parseJSON)
import           Data.Aeson.Types              (defaultOptions,
                                                fieldLabelModifier)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           Servant.Auth.Server           (FromJWT, ToJWT)

import           PatchGirl.Web.CaseInsensitive
import           PatchGirl.Web.Id


-- * whoami


{-
what will be sent from the server to the browser
-}

data Session = VisitorSession
    { _sessionAccountId :: Id Account
    , _sessionCsrfToken :: Text
    }
    | SignedUserSession
    { _sessionAccountId       :: Id Account
    , _sessionCsrfToken       :: Text
    , _sessionGithubEmail     :: Maybe String
    , _sessionGithubAvatarUrl :: String
    }
    deriving (Eq, Show, Generic)

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

data CookieSession = VisitorCookie
    { _cookieAccountId :: Id Account
    }
    | SignedUserCookie
    { _cookieAccountId       :: Id Account
    , _cookieGithubEmail     :: Maybe CaseInsensitive
    , _cookieGithubAvatarUrl :: String
    }
    deriving (Eq, Show, Generic)

instance ToJSON CookieSession where
  toJSON =
    genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON CookieSession where
  parseJSON =
    genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

instance ToJWT CookieSession
instance FromJWT CookieSession
