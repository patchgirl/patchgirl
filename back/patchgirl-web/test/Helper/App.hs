{-# LANGUAGE OverloadedStrings #-}

module Helper.App (Test(..), cleanDBAndCreateAccount, withClient, try, errorsWithStatus, defaultEnv, defaultEnv2, mkToken, signedUserToken, visitorToken, cleanDBBefore, withAccountAndToken, signedUserToken1, visitorId) where

import           Control.Concurrent.STM
import           Control.Exception                (throwIO)
import           Control.Monad                    (void)
import           Control.Monad.Reader             (runReaderT)
import qualified Data.ByteString.Lazy             as BSL
import           Data.Functor                     ((<&>))
import qualified Data.Maybe                       as Maybe
import           Data.Text                        (Text)
import           Data.Time                        (UTCTime)
import           Data.UUID                        (UUID)
import qualified Data.UUID                        as UUID
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types (Identifier (..))
import           DBUtil
import qualified Network.HTTP.Client              as Client
import           Network.HTTP.Types               (Status)
import           Network.Wai.Handler.Warp         (testWithApplication)
import qualified Say
import           Servant
import qualified Servant.Auth.Client              as Auth
import qualified Servant.Auth.Server              as Auth (defaultJWTSettings,
                                                           makeJWT, readKey)
import           Servant.Client
import           System.FilePath                  ((</>))
import qualified Test.Hspec                       as Hspec

import           PatchGirl.Web.CaseInsensitive
import           PatchGirl.Web.DB
import           PatchGirl.Web.Internal.Env
import           PatchGirl.Web.Session.Model


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
    flip Hspec.aroundWith innerSpec $ \(action :: ClientEnv -> IO()) httpManager ->
      testWithApplication app $ \port -> do
        let testBaseUrl = BaseUrl Http "localhost" port ""
        action $ ClientEnv httpManager testBaseUrl Nothing defaultMakeClientRequest


-- ** user


withAccountAndToken :: Int -> Connection -> IO (UUID, Auth.Token)
withAccountAndToken githubId connection = do
  accountId <- insertFakeAccount githubId connection
  token <- signedUserToken accountId
  return (accountId, token)

signedUserToken1 :: IO (Auth.Token, UUID)
signedUserToken1 = do
  let
    id = Maybe.fromJust $ UUID.fromString "b644ca57-7181-4f0e-a253-39ce45f5364e"
    cookieSession =
        SignedUserCookie { _cookieAccountId    = id
                         , _cookieGithubEmail = Just $ CaseInsensitive "foo@mail.com"
                         , _cookieGithubAvatarUrl = "https://foo.com/someAvatar.jpg"
                         }
  mkToken cookieSession Nothing <&> \token -> (token, id)

signedUserToken :: UUID -> IO Auth.Token
signedUserToken id = do
  let cookieSession =
        SignedUserCookie { _cookieAccountId    = id
                         , _cookieGithubEmail = Just $ CaseInsensitive "foo@mail.com"
                         , _cookieGithubAvatarUrl = "https://foo.com/someAvatar.jpg"
                         }
  mkToken cookieSession Nothing

visitorId :: UUID
visitorId =
  Maybe.fromJust $ UUID.fromString "00000000-0000-1000-a000-000000000000"

visitorToken :: IO (Auth.Token, UUID)
visitorToken = do
  let
    id = Maybe.fromJust $ UUID.fromString "00000000-0000-1000-a000-000000000000"
    cookieSession = VisitorCookie { _cookieAccountId = id }
  mkToken cookieSession Nothing <&> \token -> (token, id)


mkToken :: CookieSession -> Maybe UTCTime -> IO Auth.Token
mkToken cookieSession mexp = do
  key <- Auth.readKey $ _envAppKeyFilePath defaultEnv
  Right token <- Auth.makeJWT cookieSession (Auth.defaultJWTSettings key) mexp
  return $ Auth.Token $ BSL.toStrict token


-- * config


defaultEnv :: Env
defaultEnv =
  Env { _envPort = 3001
      , _envAppKeyFilePath = ".." </> ".appKey.test"
      , _envDB = DBConfig { _dbPort = 5432
                         , _dbName = "test"
                         , _dbUser = "postgres"
                         , _dbPassword = ""
                         }
      , _envGithub = GithubConfig { _githubConfigClientId    = "whatever"
                                 , _githubConfigClientSecret = "whatever"
                                 }
      , _envLog = Say.sayString
      , _envResetVisitorData = ""
      }


defaultEnv2 :: IO Env
defaultEnv2 = do
  logs <- newTVarIO ""
  let logFunc msg = atomically $ modifyTVar logs (++ ("\n" ++ msg))
  let file = ".." </> "reset-visitor-data.sql"
  resetVisitorData <- readFile file <&> show <&> read @Query
  return $
    Env { _envPort = 3001
        , _envAppKeyFilePath = ".." </> ".appKey.test"
        , _envDB = DBConfig { _dbPort = 5432
                           , _dbName = "test"
                           , _dbUser = "postgres"
                           , _dbPassword = ""
                           }
        , _envGithub = GithubConfig { _githubConfigClientId    = "whatever"
                                    , _githubConfigClientSecret = "whatever"
                                    }
        , _envLog = logFunc
        , _envResetVisitorData = resetVisitorData
      }


-- * db


cleanDBBefore :: (Connection -> IO a) -> IO a
cleanDBBefore f = do
  connection <- runReaderT getDBConnection defaultEnv
  listTables connection >>= mapM_ (truncateTable connection)
  f connection


data Test = Test
    { connection :: Connection
    , accountId  :: UUID
    , token      :: Auth.Token
    }

cleanDBAndCreateAccount :: (Test -> IO a) -> IO a
cleanDBAndCreateAccount f = do
  connection <- runReaderT getDBConnection defaultEnv
  listTables connection >>= mapM_ (truncateTable connection)
  accountId <- insertFakeAccount 1 connection
  token <- signedUserToken accountId
  f Test { connection = connection
         , accountId = accountId
         , token = token
         }

listTables :: Connection -> IO [Text]
listTables c =
  map fromOnly `fmap` query_ c q
  where
    q = mconcat [ " SELECT c.relname FROM pg_catalog.pg_class c"
                , " LEFT JOIN pg_catalog.pg_namespace n"
                , " ON c.relnamespace = n.oid"
                , " WHERE c.relkind IN ('r', '')"
                , " AND n.nspname <> 'pg_catalog'"
                , " AND n.nspname <> 'information_schema'"
                , " AND n.nspname !~ '^pg_toast'"
                , " AND pg_catalog.pg_table_is_visible(c.oid)"
                ]

truncateTable :: Connection -> Text -> IO ()
truncateTable c =
  void . execute c q . Only . Identifier
  where
    q = "TRUNCATE ? CASCADE"
