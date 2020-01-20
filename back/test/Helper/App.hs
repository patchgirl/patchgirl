{-# LANGUAGE OverloadedStrings #-}

module Helper.App (withClient, try, errorsWithStatus, defaultConfig, mkToken, signedUserToken, visitorToken, cleanDBAfter) where

import           Control.Monad                    (void)
import           Control.Monad.Reader             (runReaderT)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types (Identifier (..))
import           DB                               (getDBConnection)

import           Config
import           Control.Exception                (finally, throwIO)
import           Control.Monad.Trans              (liftIO)
import           Crypto.JOSE                      as Jose
import qualified Data.ByteString.Lazy             as BSL
import           Data.ByteString.UTF8             as BSU
import           Data.Time                        (UTCTime, defaultTimeLocale,
                                                   parseTimeOrError)
import           Model
import           Network.HTTP.Client              (defaultManagerSettings,
                                                   newManager)
import           Network.HTTP.Types               (Status)
import           Network.Wai.Handler.Warp         (testWithApplication)
import           Servant
import           Servant.Auth.Client
import           Servant.Auth.Server              (defaultJWTSettings,
                                                   fromSecret, generateKey,
                                                   makeJWT, readKey)
import           Servant.Client
import           Session.Model
import           Test.Hspec                       (SpecWith, aroundWith,
                                                   beforeAll)


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
  beforeAll (newManager defaultManagerSettings) $
    flip aroundWith innerSpec $ \action httpManager ->
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
