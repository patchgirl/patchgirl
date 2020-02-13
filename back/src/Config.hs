{-# LANGUAGE DeriveGeneric #-}

module Config where

import           Data.Text (Text)
import           Dhall

importConfig :: IO Config
importConfig =
  input auto "./config.dhall"


-- * mailgun


data MailgunConfig
  = MailgunConfig { domain      :: Text
                  , apiKey      :: Text
                  , authorEmail :: Text
                  }
  deriving (Generic, Show)

instance FromDhall MailgunConfig


-- * db


data DBConfig
  = DBConfig { dbPort     :: Natural
             , dbName     :: Text
             , dbUser     :: Text
             , dbPassword :: Text
             }
  deriving (Generic, Show)

instance FromDhall DBConfig

-- * config


data Config
  = Config { port           :: Natural
           , appKeyFilePath :: String
           , dbConfig       :: DBConfig
           , mailgun        :: MailgunConfig
           }
  deriving (Generic, Show)

instance FromDhall Config
