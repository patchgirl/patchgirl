{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Dhall

importConfig :: IO Config
importConfig =
  input auto "./config.dhall"

data MailgunConfig
  = MailgunConfig { domain :: Text
                  , apiKey :: Text
                  }
  deriving (Generic, Show)

data Config
  = Config { mailgun :: MailgunConfig
           }
  deriving (Generic, Show)

instance FromDhall Config
instance FromDhall MailgunConfig