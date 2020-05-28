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
          , Mode(..)
          ) where

import qualified Control.Lens             as Lens
import qualified Data.ByteString.UTF8     as BSU
import           Data.Text                (Text)
import           Dhall                    (Natural)
import qualified Dhall
import           GHC.Generics             (Generic)
import qualified Network.HTTP.Client      as Http
import           RequestComputation.Model

data Mode = DesktopMode | WebMode

createEnv :: Mode -> (String -> IO ()) -> (Http.Request -> IO (HttpResponse BSU.ByteString)) -> IO Env
createEnv whichConfig log httpRequest = do
  globalConfig :: GlobalConfig <- Dhall.input Dhall.auto "./config.dhall"
  let Config{..} = chooseConfig globalConfig whichConfig
  return $ Env { _envPort = _configPort
               , _envAppKeyFilePath = _configAppKeyFilePath
               , _envDB = _configDB
               , _envGithub = _configGithub
               , _envLog = log
               , _envHttpRequest = httpRequest
               }
  where
    chooseConfig :: GlobalConfig -> Mode -> Config
    chooseConfig GlobalConfig { desktopConfig, webConfig } = \case
      DesktopMode -> desktopConfig
      WebMode -> webConfig

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
           , _configRunnerUrl      :: String
           }
  deriving (Generic, Show)

instance Dhall.FromDhall Config where
  autoWith _ = Dhall.record $
    Config
      <$> Dhall.field "port" Dhall.natural
      <*> Dhall.field "appKeyFilePath" Dhall.string
      <*> Dhall.field "db" Dhall.auto
      <*> Dhall.field "github" Dhall.auto
      <*> Dhall.field "runnerUrl" Dhall.string


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
