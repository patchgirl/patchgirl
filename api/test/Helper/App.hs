module Helper.App (withClient, try, errorsWithStatus) where

import           Servant
import           Servant.Client
import           Test.Hspec (SpecWith, aroundWith, beforeAll)
import           Network.HTTP.Types (Status)
import           Network.Wai.Handler.Warp (testWithApplication)
import           Network.HTTP.Client              (Manager,
                                                   defaultManagerSettings,
                                                   newManager)
import           Control.Exception                (throwIO)


--import           Test.Hspec


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
