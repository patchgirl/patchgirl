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
import           Dhall                    (Natural)
import qualified Dhall
import           GHC.Generics             (Generic)
import qualified Network.HTTP.Client      as Http
import           RequestComputation.Model


createEnv :: (String -> IO ()) -> (Http.Request -> IO (HttpResponse BSU.ByteString)) -> IO Env
createEnv log httpRequest = do
  Config{..} <- Dhall.input Dhall.auto "./config.dhall"
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

instance Dhall.FromDhall DBConfig where
  autoWith _ = Dhall.record $
    DBConfig <$> Dhall.field "port" Dhall.natural <*> Dhall.field "name" Dhall.strictText <*> Dhall.field "user" Dhall.strictText <*> Dhall.field "password" Dhall.strictText


-- * github


data GithubConfig
  = GithubConfig { _githubConfigClientId     :: Text
                 , _githubConfigClientSecret :: Text
                 }
  deriving (Generic, Show)

instance Dhall.FromDhall GithubConfig where
  autoWith _ = Dhall.record $
    GithubConfig <$> Dhall.field "clientId" Dhall.strictText <*> Dhall.field "clientSecret" Dhall.strictText


-- * config


data Config
  = Config { _configPort           :: Natural
           , _configAppKeyFilePath :: String
           , _configDB             :: DBConfig
           , _configGithub         :: GithubConfig
           }
  deriving (Generic, Show)

instance Dhall.FromDhall Config where
  autoWith _ = Dhall.record $
    Config <$> Dhall.field "port" Dhall.natural <*> Dhall.field "appKeyFilePath" Dhall.string <*> Dhall.field "db" Dhall.auto <*> Dhall.field "github" Dhall.auto


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
