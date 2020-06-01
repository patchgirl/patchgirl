{-# LANGUAGE DeriveGeneric #-}

module Env( createEnv
          , Env(..)
          , envPort
          , envAppKeyFilePath
          , envDB
          , envGithub
          , envLog
          , envFrontConfig
          , DBConfig(..)
          , GithubConfig(..)
          , Config(..)
          , FrontConfig(..)
          , getConfig
          ) where

import qualified Control.Lens         as Lens
import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.UTF8 as BSU
import           Data.Text            (Text)
import           Dhall                (Natural)
import qualified Dhall
import           GHC.Generics         (Generic)
import qualified Network.HTTP.Client  as Http


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


-- * global config (desktop and web)


data GlobalConfig
  = GlobalConfig { desktopConfig :: Config
                 , webConfig     :: Config
                 }
  deriving (Generic, Show)

instance Dhall.FromDhall GlobalConfig where
  autoWith _ = Dhall.record $
    GlobalConfig <$> Dhall.field "desktop" Dhall.auto <*> Dhall.field "web" Dhall.auto


-- * config


data Config
  = Config { _configPort           :: Natural
           , _configAppKeyFilePath :: String
           , _configDB             :: DBConfig
           , _configGithub         :: GithubConfig
           , _configFrontConfig    :: FrontConfig
           }
  deriving (Generic, Show)

instance Dhall.FromDhall Config where
  autoWith _ = Dhall.record $
    Config
      <$> Dhall.field "port" Dhall.natural
      <*> Dhall.field "appKeyFilePath" Dhall.string
      <*> Dhall.field "db" Dhall.auto
      <*> Dhall.field "github" Dhall.auto
      <*> Dhall.field "frontConfig" Dhall.auto

getConfig :: IO Config
getConfig = do
  Dhall.input Dhall.auto "./config.dhall"


-- * front config


data FrontConfig
  = FrontConfig { _frontConfigRunnerUrl :: String
                }
  deriving (Generic, Show)

instance Dhall.FromDhall FrontConfig where
  autoWith _ = Dhall.record $
    FrontConfig
      <$> Dhall.field "runnerUrl" Dhall.auto

instance Aeson.ToJSON FrontConfig where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON FrontConfig where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }



-- * env


data Env
  = Env { _envPort           :: Natural
        , _envAppKeyFilePath :: String
        , _envDB             :: DBConfig
        , _envGithub         :: GithubConfig
        , _envLog            :: String -> IO ()
        , _envFrontConfig    :: FrontConfig
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
               , _envFrontConfig = _configFrontConfig
               }
