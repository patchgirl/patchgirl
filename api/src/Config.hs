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


-- * mode


data Mode
  = Dev
  | Test
  deriving (Generic, Show)

instance FromDhall Mode


-- * config


data Config
  = Config { mode           :: Mode
           , port           :: Natural
           , appKeyFilePath :: String
           , mailgun        :: MailgunConfig
           }
  deriving (Generic, Show)

instance FromDhall Config
