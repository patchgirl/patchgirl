
module AppSpec where

import           Control.Exception (throwIO)
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.Types
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Test.Hspec

import           App hiding (getItems)

getItems :: ClientM [Item]
getItem :: Integer -> ClientM Item
getItems :<|> getItem = client itemApi

spec :: Spec
spec = do
  describe "/item" $ do
    withClient mkApp $ do
      it "lists an example item" $ \ env -> do
        try env getItems `shouldReturn` [Item 0 "example item"]

      it "allows to show items by id" $ \ env -> do
        try env (getItem 0) `shouldReturn` Item 0 "example item"

      it "throws a 404 for missing items" $ \ env -> do
        try env (getItem 42) `shouldThrow` errorsWithStatus notFound404

errorsWithStatus :: Status -> ServantError -> Bool
errorsWithStatus status servantError = case servantError of
  FailureResponse response -> responseStatusCode response == status
  _ -> False

withClient :: IO Application -> SpecWith ClientEnv -> SpecWith ()
withClient x innerSpec =
  beforeAll (newManager defaultManagerSettings) $ do
    flip aroundWith innerSpec $ \ action -> \ httpManager -> do
      testWithApplication x $ \ port -> do
        let testBaseUrl = BaseUrl Http "localhost" port ""
        action (ClientEnv httpManager testBaseUrl Nothing)

type Host = (Manager, BaseUrl)

try :: ClientEnv -> ClientM a -> IO a
try clientEnv action = either throwIO return =<<
  runClientM action clientEnv
