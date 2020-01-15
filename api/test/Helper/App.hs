{-# LANGUAGE OverloadedStrings #-}

module Helper.App (withClient, try, errorsWithStatus, defaultConfig, mkToken, signedUserToken, visitorToken) where

import           Config
import           Control.Exception        (throwIO)
import           Control.Monad.Trans      (liftIO)
import           Crypto.JOSE              as Jose
import qualified Data.ByteString.Lazy     as BSL
import           Data.ByteString.UTF8     as BSU
import           Data.Time                (UTCTime, defaultTimeLocale,
                                           parseTimeOrError)
import           Model
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Network.HTTP.Types       (Status)
import           Network.Wai.Handler.Warp (testWithApplication)
import           Servant
import           Servant.Auth.Client
import           Servant.Auth.Server      (defaultJWTSettings, fromSecret,
                                           generateKey, makeJWT, readKey)
import           Servant.Client
import           Session.Model
import           Test.Hspec               (SpecWith, aroundWith, beforeAll)


-- * helper


try :: ClientEnv -> ClientM a -> IO a
try clientEnv action =
  either throwIO return =<< runClientM action clientEnv

errorsWithStatus :: Status -> ClientError -> Bool
errorsWithStatus status servantError =
  case servantError of
    FailureResponse _ response -> responseStatusCode response == status
    _                          -> False

withClient :: IO Application -> SpecWith ClientEnv -> SpecWith ()
withClient app innerSpec =
  beforeAll (newManager defaultManagerSettings) $ do
    flip aroundWith innerSpec $ \ action -> \ httpManager -> do
      testWithApplication app $ \ port -> do
        let testBaseUrl = BaseUrl Http "localhost" port ""
        action (ClientEnv httpManager testBaseUrl Nothing)

signedUserToken :: Int -> IO Token
signedUserToken id = do
  let cookieSession =
        SignedUserCookie { _cookieAccountId    = id
                         , _cookieAccountEmail = CaseInsensitive "foo@mail.com"
                         }
  mkToken cookieSession Nothing

visitorToken :: IO Token
visitorToken = do
  let cookieSession = VisitorCookie { _cookieAccountId = 1 }
  mkToken cookieSession Nothing


mkToken :: CookieSession -> Maybe UTCTime -> IO Token
mkToken cookieSession mexp = do
  key <- readKey $ appKeyFilePath defaultConfig
  Right token <- makeJWT cookieSession (defaultJWTSettings key) mexp
  return $ Token $ BSL.toStrict token


-- * config


defaultConfig :: Config
defaultConfig =
  Config { port    = 3001
         , appKeyFilePath = ".appKey.test"
         , mailgun = MailgunConfig { domain      = "whatever"
                                   , apiKey      = "whatever"
                                   , authorEmail = "admin@mail.com"
                                   }
         }
