{-# LANGUAGE OverloadedStrings #-}

module Helper.App (withClient, try, errorsWithStatus, defaultConfig, mkToken, signedUserToken, visitorToken, cleanDBAfter, withAccountAndToken, signedUserToken1, visitorId) where

import           Control.Monad                    (void)
import           Control.Monad.Reader             (runReaderT)
import           Data.Functor                     ((<&>))
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types (Identifier (..))
import           DB                               (getDBConnection)

import           Account.DB
import           Config
import           Control.Exception                (finally, throwIO)
import qualified Data.ByteString.Lazy             as BSL
import qualified Data.Maybe                       as Maybe
import           Data.Time                        (UTCTime)
import           Data.UUID                        (UUID)
import qualified Data.UUID                        as UUID
import           Model
import           Network.HTTP.Client              (defaultManagerSettings,
                                                   newManager)
import           Network.HTTP.Types               (Status)
import           Network.Wai.Handler.Warp         (testWithApplication)
import           Servant
import qualified Servant.Auth.Client              as Auth
import qualified Servant.Auth.Server              as Auth (defaultJWTSettings,
                                                           makeJWT, readKey)
import           Servant.Client
import           Session.Model
import           Test.Hspec                       (SpecWith, aroundWith,
                                                   beforeAll)


-- * helper


withAccountAndToken :: NewFakeAccount -> Connection -> IO (UUID, Auth.Token)
withAccountAndToken newFakeAccount connection = do
  (accountId, _) <- insertFakeAccount newFakeAccount connection
  token <- signedUserToken accountId
  return (accountId, token)


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
  beforeAll (newManager defaultManagerSettings) $
    flip aroundWith innerSpec $ \action httpManager ->

      testWithApplication app $ \ port -> do
        let testBaseUrl = BaseUrl Http "localhost" port ""
        action (ClientEnv httpManager testBaseUrl Nothing)

signedUserToken1 :: IO (Auth.Token, UUID)
signedUserToken1 = do
  let
    id = Maybe.fromJust $ UUID.fromString "b644ca57-7181-4f0e-a253-39ce45f5364e"
    cookieSession =
        SignedUserCookie { _cookieAccountId    = id
                         , _cookieAccountEmail = CaseInsensitive "foo@mail.com"
                         }
  mkToken cookieSession Nothing <&> \token -> (token, id)

signedUserToken :: UUID -> IO Auth.Token
signedUserToken id = do
  let cookieSession =
        SignedUserCookie { _cookieAccountId    = id
                         , _cookieAccountEmail = CaseInsensitive "foo@mail.com"
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
  key <- Auth.readKey $ appKeyFilePath defaultConfig
  Right token <- Auth.makeJWT cookieSession (Auth.defaultJWTSettings key) mexp
  return $ Auth.Token $ BSL.toStrict token


-- * config


defaultConfig :: Config
defaultConfig =
  Config { port = 3001
         , appKeyFilePath = ".appKey.test"
         , dbConfig = DBConfig { dbPort = 5432
                               , dbName = "test"
                               , dbUser = "postgres"
                               , dbPassword = ""
                               }
         , mailgun = MailgunConfig { domain      = "whatever"
                                   , apiKey      = "whatever"
                                   , authorEmail = "admin@mail.com"
                                   }
         }


-- * db


cleanDBAfter :: (Connection -> IO a) -> IO a
cleanDBAfter f = do
  connection <- runReaderT getDBConnection defaultConfig
  withConnection f connection

withConnection :: (Connection -> IO a) -> Connection -> IO a
withConnection f connection =
  finally (f connection) $ listTables connection >>= mapM_ (truncateTable connection)


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
