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
                                           generateKey, makeJWT)
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
  let secret = fromSecret $ BSU.fromString $ appKey defaultConfig
  Right token <- makeJWT cookieSession (defaultJWTSettings secret) mexp
  return $ Token $ BSL.toStrict token


-- * config


defaultConfig :: Config
defaultConfig =
  Config { port    = 3001
         , appKey = "\65533z\65533\65533r\65533\rP\DEL\65533\DLE\65533\65533\DC1\65533\tQ\65533\65533\65533\65533\65533\n\ETB\65533\65533\"\65533\DC4\65533\65533?\65533J\65533\65533\65533\ESC9\65533S\1116@m.\n\65533\65533\65533\567\65533\65533`\65533+\65533\65533\65533\65533\CAN\ETB]'iR\65533`\65533\65533[\f3\65533Oz\65533V\65533GF|*\EMn\65533B\65533?\SYN\65533\&6\65533\65533\65533A\65533\DEL\65533\65533\DC4e\65533+J#g\65533-1\65533\b<\65533\f\US\65533\65533c\65533\NUL\65533\v^co\65533w\65533\65533P\DC1Xl\65533\65533\DEL<\65533\65533\t\65533\65533\b\65533\65533\65533\&9N\EMW\bx\\\65533_\65533L\65533:U\65533\RS\65533\65533\65533\DC2\1243|n\r!7\65533PE\65533\SUB(\65533r\GS\EM\65533\b%\65533@\65533I\65533\65533v\USO\65533-d\65533\65533\65533\1764\DC2\65533\SUB-\65533\65533-1\USWn\65533\65533\65533\65533a]\65533b \65533\65533\65533\65533]}\"\65533\EM%\65533\65533\&3\65533y\1382\&0B\65533V3\65533\65533\65533"
         , mailgun = MailgunConfig { domain      = "whatever"
                                   , apiKey      = "whatever"
                                   , authorEmail = "whatever"
                                   }
         }
