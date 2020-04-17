{-# LANGUAGE DeriveGeneric #-}

module Config where

import           Data.Text (Text)
import           Dhall

importConfig :: IO Config
importConfig =
  input auto "./config.dhall"


-- * db


data DBConfig
  = DBConfig { dbPort     :: Natural
             , dbName     :: Text
             , dbUser     :: Text
             , dbPassword :: Text
             }
  deriving (Generic, Show)

instance FromDhall DBConfig


-- * github


data GithubConfig
  = GithubConfig { githubConfigClientId     :: Text
                 , githubConfigClientSecret :: Text
                 }
  deriving (Generic, Show)

instance FromDhall GithubConfig


-- * config


data Config
  = Config { port           :: Natural
           , appKeyFilePath :: String
           , dbConfig       :: DBConfig
           , githubConfig   :: GithubConfig
           }
  deriving (Generic, Show)

instance FromDhall Config

{-
data Env
  = Env { logConfigLog :: (String -> IO ())
        ,
        }
-}
