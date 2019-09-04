
module AppSpec where

import           Control.Exception        (throwIO)
import           Network.HTTP.Client      (Manager, defaultManagerSettings,
                                           newManager)
import           Network.HTTP.Types
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Test.Hspec

import           App
import           Request                  hiding (getRequests)

getRequests :: ClientM [Request]
getRequest :: Integer -> ClientM Request
getRequests :<|> getRequest = client requestApi

spec :: Spec
spec = do
  describe "/request" $ do
    withClient mkApp $ do
      it "lists an example request" $ \ env -> do
        try env getRequests `shouldReturn` [Request 0 "example request"]

      it "allows to show requests by id" $ \ env -> do
        try env (getRequest 0) `shouldReturn` Request 0 "example request"

      it "throws a 404 for missing requests" $ \ env -> do
        try env (getRequest 42) `shouldThrow` errorsWithStatus notFound404

errorsWithStatus :: Status -> ClientError -> Bool
errorsWithStatus status servantError = case servantError of
  FailureResponse _ response -> responseStatusCode response == status
  _                          -> False

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
