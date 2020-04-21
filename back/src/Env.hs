{-# LANGUAGE DeriveGeneric #-}

module Env( createEnv
          , Env(..)
          , envPort
          , envAppKeyFilePath
          , envDB
          , envGithub
          , envLog
          , envHttpRequest
          , DBConfig(..)
          , GithubConfig(..)
          ) where

import qualified Control.Lens             as Lens
import qualified Data.ByteString.UTF8     as BSU
import           Data.Text                (Text)
import           Dhall
import qualified Network.HTTP.Client      as Http
import           RequestComputation.Model


createEnv :: (String -> IO ()) -> (Http.Request -> IO (HttpResponse BSU.ByteString)) -> IO Env
createEnv log httpRequest = do
  Config{..} <- input auto "./config.dhall"
  return $ Env { _envPort = _configPort
               , _envAppKeyFilePath = _configAppKeyFilePath
               , _envDB = _configDB
               , _envGithub = _configGithub
               , _envLog = log
               , _envHttpRequest = httpRequest
               }


-- * db


data DBConfig
  = DBConfig { _dbPort     :: Natural
             , _dbName     :: Text
             , _dbUser     :: Text
             , _dbPassword :: Text
             }
  deriving (Generic, Show)

instance FromDhall DBConfig


-- * github


data GithubConfig
  = GithubConfig { _githubConfigClientId     :: Text
                 , _githubConfigClientSecret :: Text
                 }
  deriving (Generic, Show)

instance FromDhall GithubConfig


-- * config


data Config
  = Config { _configPort           :: Natural
           , _configAppKeyFilePath :: String
           , _configDB             :: DBConfig
           , _configGithub         :: GithubConfig
           }
  deriving (Generic, Show)

instance FromDhall Config


-- * env


data Env
  = Env { _envPort           :: Natural
        , _envAppKeyFilePath :: String
        , _envDB             :: DBConfig
        , _envGithub         :: GithubConfig
        , _envLog            :: String -> IO ()
        , _envHttpRequest    :: Http.Request -> IO (HttpResponse BSU.ByteString)
        }

$(Lens.makeLenses ''Env)
