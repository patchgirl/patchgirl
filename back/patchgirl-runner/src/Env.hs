{-# LANGUAGE DeriveGeneric #-}

module Env( createEnv
          , Env(..)
          , envPort
          , envLog
          , envHttpRequest
          , Config(..)
          , getConfig
          ) where

import qualified Control.Lens             as Lens
import qualified Data.ByteString.UTF8     as BSU
import           Dhall                    (Natural)
import qualified Dhall
import           GHC.Generics             (Generic)
import qualified Network.HTTP.Client      as Http

import           RequestComputation.Model


-- * config


newtype Config
  = Config { _configPort :: Natural }
  deriving (Generic, Show)

instance Dhall.FromDhall Config where
  autoWith _ = Dhall.record $
    Config
      <$> Dhall.field "port" Dhall.auto

getConfig :: IO Config
getConfig = do
  Dhall.input Dhall.auto "./runner.dhall"


-- * env


data Env
  = Env { _envPort        :: Natural
        , _envLog         :: String -> IO ()
        , _envHttpRequest :: Http.Request -> IO (HttpResponse BSU.ByteString)
        }

$(Lens.makeLenses ''Env)


-- * create env


createEnv :: (String -> IO ()) -> (Http.Request -> IO (HttpResponse BSU.ByteString)) -> IO Env
createEnv log httpRequest = do
  return $ Env { _envPort = 37465
               , _envLog = log
               , _envHttpRequest = httpRequest
               }
