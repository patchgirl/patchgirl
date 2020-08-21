{-# LANGUAGE DeriveGeneric #-}

module Env( createEnv
          , Env(..)
          , envPort
          , envLog
          , envPgRunner
          , envHttpRequest
          , Config(..)
          , getConfig
          ) where

import qualified Control.Lens              as Lens
import qualified Data.ByteString.UTF8      as BSU
import qualified Database.PostgreSQL.LibPQ as LibPQ
import           Dhall                     (Natural)
import qualified Dhall
import           GHC.Generics              (Generic)
import qualified Network.HTTP.Client       as Http

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
        , _envPgRunner :: LibPQ.Connection -> BSU.ByteString -> IO (Maybe LibPQ.Result)
        }

$(Lens.makeLenses ''Env)


-- * create env


createEnv
  :: (String -> IO ())
  -> (Http.Request -> IO (HttpResponse BSU.ByteString))
  -> (LibPQ.Connection -> BSU.ByteString -> IO (Maybe LibPQ.Result))
  -> IO Env
createEnv log httpRequest pgRunner = do
  return $ Env { _envPort = 37465
               , _envLog = log
               , _envHttpRequest = httpRequest
               , _envPgRunner = pgRunner
               }
