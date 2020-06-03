{-# LANGUAGE OverloadedStrings #-}

module Helper.App (withClient, try, errorsWithStatus, defaultEnv, defaultEnv2) where

import           Control.Concurrent.STM
import           Control.Exception        (throwIO)
import qualified Network.HTTP.Client      as Client
import           Network.HTTP.Types       (Status)
import           Network.Wai.Handler.Warp (testWithApplication)
import           Servant
import           Servant.Client
import qualified Test.Hspec               as Hspec

import           Env


-- * helper


-- ** servant


try :: ClientEnv -> ClientM a -> IO a
try clientEnv action =
  either throwIO return =<< runClientM action clientEnv

errorsWithStatus :: Status -> ClientError -> Bool
errorsWithStatus status servantError =
  case servantError of
    FailureResponse _ response -> responseStatusCode response == status
    _                          -> False

withClient :: IO Application -> Hspec.SpecWith ClientEnv -> Hspec.SpecWith ()
withClient app innerSpec =
  Hspec.beforeAll (Client.newManager Client.defaultManagerSettings) $
    flip Hspec.aroundWith innerSpec $ \action httpManager ->
      testWithApplication app $ \port -> do
        let testBaseUrl = BaseUrl Http "localhost" port ""
        action (ClientEnv httpManager testBaseUrl Nothing)


-- ** user


-- * config


defaultEnv :: Env
defaultEnv =
  Env { _envPort = 3001
      , _envLog = undefined
      , _envHttpRequest = undefined
      }


defaultEnv2 :: IO Env
defaultEnv2 = do
  logs <- newTVarIO ""
  let logFunc msg = atomically $ modifyTVar logs (++ ("\n" ++ msg))
  return $
    Env { _envPort = 3001
        , _envLog = logFunc
        , _envHttpRequest = undefined
        }