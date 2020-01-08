{-# LANGUAGE OverloadedStrings #-}

module Helper.App (withClient, try, errorsWithStatus, defaultConfig) where

import           Config
import           Control.Exception        (throwIO)
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Network.HTTP.Types       (Status)
import           Network.Wai.Handler.Warp (testWithApplication)
import           Servant
import           Servant.Client
import           Test.Hspec               (SpecWith, aroundWith, beforeAll)

try :: ClientEnv -> ClientM a -> IO a
try clientEnv action =
  either throwIO return =<< runClientM action clientEnv

errorsWithStatus :: Status -> ClientError -> Bool
errorsWithStatus status servantError =
  case servantError of
    FailureResponse _ response -> responseStatusCode response == status
    _                          -> False

withClient :: IO Application -> SpecWith ClientEnv -> SpecWith ()
withClient x innerSpec =
  beforeAll (newManager defaultManagerSettings) $ do
    flip aroundWith innerSpec $ \ action -> \ httpManager -> do
      testWithApplication x $ \ port -> do
        let testBaseUrl = BaseUrl Http "localhost" port ""
        action (ClientEnv httpManager testBaseUrl Nothing)

defaultConfig :: Config
defaultConfig =
  Config { port    = 3001
         , mailgun = MailgunConfig { domain      = "whatever"
                                   , apiKey      = "whatever"
                                   , authorEmail = "whatever"
                                   }
         }
