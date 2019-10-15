{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module AppSpec where

import           App
import           Control.Concurrent
import           Control.Concurrent
import           Control.Exception                (bracket, finally, throwIO)
import           Control.Monad                    (void)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.Types (Identifier (..))
import           DB
import           Network.HTTP.Client              (Manager,
                                                   defaultManagerSettings,
                                                   newManager)
import           Network.HTTP.Types
import           Network.Wai                      (Application)
import           Network.Wai.Handler.Warp
import           RequestCollection                          hiding (getRequests)
import           Servant
import           Servant.Client
import           Test.Hspec

withTestTransaction :: (Connection -> IO a) -> IO a
withTestTransaction f =
  getDBConnection >>= withConnection f

withConnection :: (Connection -> IO a) -> Connection -> IO a
withConnection f connection =
  finally (f connection) $ listTables connection >>= mapM_ (truncateTable connection)

listTables :: Connection -> IO [Text]
listTables c = map fromOnly `fmap` query_ c q
  where
    q = mconcat [ "SELECT c.relname FROM pg_catalog.pg_class c"
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

getRequests :: ClientM [Request]
getRequest :: Int -> ClientM Request
getRequests :<|> getRequest =
  client api

insertRequest :: Connection -> String -> IO Int
insertRequest connection requestText = do
  [Only id] <- query connection rawQuery (Only requestText :: Only String)
  return id
  where
    rawQuery = [sql|
                   INSERT INTO request (text)
                   VALUES (?)
                   RETURNING id
                   |]

spec :: Spec
spec = do
  describe "GET /requests/:id" $ do
    withClient mkApp $ do
      it "return request by id" $ \clientEnv ->
        withTestTransaction $ \connection -> do
          id <- insertRequest connection "test"
          try clientEnv (getRequest id) `shouldReturn` Request id "test"

      it "return 404 for missing requests" $ \clientEnv -> do
        try clientEnv (getRequest 1) `shouldThrow` errorsWithStatus notFound404

  describe "GET /requests" $ do
    withClient mkApp $ do
      it "return all requests" $ \clientEnv ->
        withTestTransaction $ \connection -> do
          id1 <- insertRequest connection "test1"
          id2 <- insertRequest connection "test2"
          let expectedRes =
                [ Request id1 "test1"
                , Request id2 "test2"
                ]
          try clientEnv getRequests `shouldReturn` expectedRes

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
