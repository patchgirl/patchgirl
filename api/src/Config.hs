{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

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


-- * config


data Config
  = Config { port           :: Natural
           , appKeyFilePath :: String
           , mailgun        :: MailgunConfig
           }
  deriving (Generic, Show)

instance FromDhall Config
