{-# LANGUAGE OverloadedStrings #-}

module Helper.App (withClient, try, errorsWithStatus, defaultEnv, defaultEnv2) where

import           Control.Concurrent.STM
import           Control.Exception                (finally, throwIO)
import           Control.Monad                    (void)
import           Control.Monad.Reader             (runReaderT)
import qualified Data.ByteString.Lazy             as BSL
import           Data.Functor                     ((<&>))
import qualified Data.Maybe                       as Maybe
import           Data.Text                        (Text)
--import           Data.Time                        (UTCTime)
import           Data.UUID                        (UUID)
import qualified Data.UUID                        as UUID
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types (Identifier (..))
--import           DBUtil
import qualified Network.HTTP.Client              as Client
import           Network.HTTP.Types               (Status)
import           Network.Wai.Handler.Warp         (testWithApplication)
import qualified Say
import           Servant
--import qualified Servant.Auth.Client              as Auth
--import qualified Servant.Auth.Server              as Auth (defaultJWTSettings,
--                                                           makeJWT, readKey)
import           Servant.Client
import qualified Test.Hspec                       as Hspec

import           Env
--import           PatchGirl.Internal               hiding (Http)


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
      , _envAppKeyFilePath = "../.appKey.test"
      , _envDB = DBConfig { _dbPort = 5432
                         , _dbName = "test"
                         , _dbUser = "postgres"
                         , _dbPassword = ""
                         }
      , _envGithub = GithubConfig { _githubConfigClientId    = "whatever"
                                 , _githubConfigClientSecret = "whatever"
                                 }
      , _envLog = Say.sayString
      , _envHttpRequest = undefined
      , _envFrontConfig = undefined
      }


defaultEnv2 :: IO Env
defaultEnv2 = do
  logs <- newTVarIO ""
  let logFunc msg = atomically $ modifyTVar logs (++ ("\n" ++ msg))
  return $
    Env { _envPort = 3001
        , _envAppKeyFilePath = "../.appKey.test"
        , _envDB = DBConfig { _dbPort = 5432
                           , _dbName = "test"
                           , _dbUser = "postgres"
                           , _dbPassword = ""
                           }
        , _envGithub = GithubConfig { _githubConfigClientId    = "whatever"
                                   , _githubConfigClientSecret = "whatever"
                                   }
        , _envLog = logFunc
        , _envHttpRequest = undefined
        , _envFrontConfig = undefined
      }
