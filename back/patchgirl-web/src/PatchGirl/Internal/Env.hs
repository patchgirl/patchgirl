{-# LANGUAGE DeriveGeneric #-}

module PatchGirl.Internal.Env
  ( createEnv
  , Env(..)
  , envPort
  , envAppKeyFilePath
  , envDB
  , envGithub
  , envLog
  , DBConfig(..)
  , GithubConfig(..)
  , Config(..)
  , getConfig
  ) where

import qualified Control.Lens as Lens
import           Data.Text    (Text)
import           Dhall        (Natural)
import qualified Dhall
import           GHC.Generics (Generic)


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
    Config
      <$> Dhall.field "port" Dhall.natural
      <*> Dhall.field "appKeyFilePath" Dhall.string
      <*> Dhall.field "db" Dhall.auto
      <*> Dhall.field "github" Dhall.auto

getConfig :: IO Config
getConfig =
  Dhall.input Dhall.auto "./web.dhall"


-- * env


data Env
  = Env { _envPort           :: Natural
        , _envAppKeyFilePath :: String
        , _envDB             :: DBConfig
        , _envGithub         :: GithubConfig
        , _envLog            :: String -> IO ()
        }

$(Lens.makeLenses ''Env)


-- * create env


createEnv :: (String -> IO ()) -> IO Env
createEnv log = do
  Config{..} <- getConfig
  return $ Env { _envPort = _configPort
               , _envAppKeyFilePath = _configAppKeyFilePath
               , _envDB = _configDB
               , _envGithub = _configGithub
               , _envLog = log
               }
