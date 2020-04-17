{-# LANGUAGE DeriveGeneric #-}

module Env(createEnv, Env(..), DBConfig(..), GithubConfig(..)) where

import           Data.Text (Text)
import           Dhall

createEnv :: (String -> IO()) -> IO Env
createEnv log = do
  Config{..} <- importConfig
  return $ Env { envPort = configPort
               , envAppKeyFilePath = configAppKeyFilePath
               , envDB = configDB
               , envLog = log
               }
    where
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
  = Config { configPort           :: Natural
           , configAppKeyFilePath :: String
           , configDB             :: DBConfig
           , configGithub         :: GithubConfig
           }
  deriving (Generic, Show)

instance FromDhall Config


-- * env


data Env
  = Env { envPort           :: Natural
        , envAppKeyFilePath :: String
        , envDB             :: DBConfig
        , envGithub         :: GithubConfig
        , envLog            :: (String -> IO ())
        }
