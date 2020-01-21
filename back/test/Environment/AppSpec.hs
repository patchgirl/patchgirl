{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

module Environment.AppSpec where

import           Account.App
import           Account.DB
import           Account.Model
import           App
import           Control.Lens        ((^.))
import           Data.Maybe          (isJust, isNothing)
import           Environment.App
import           Environment.DB
import           Helper.App
import           Model
import           Network.HTTP.Types  (badRequest400)
import           Servant
import qualified Servant.Auth.Client as Auth
import qualified Servant.Auth.Server as Auth
import           Servant.Client      (ClientM, client)
import           Session.Model
import           Test.Hspec

-- * client


createEnvironment :: Auth.Token -> NewEnvironment -> ClientM Int
createEnvironment
  :<|> getEnvironmentsHandler
  :<|> updateEnvironmentHandler
  :<|> deleteEnvironmentHandler
  :<|> updateKeyValuesHandler
  :<|> deleteKeyValueHandler =
  client (Proxy :: Proxy (PEnvironmentApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $ do


-- ** create environment


    describe "create environment" $ do
      it "should create an environment and bind it to an account" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount connection
          token <- signedUserToken accountId
          let newEnvironment = NewEnvironment { _newEnvironmentName = "test" }
          environmentId <- try clientEnv (createEnvironment token newEnvironment)
          FakeEnvironment { _fakeEnvironmentName } <- selectFakeEnvironment environmentId connection
          _fakeEnvironmentName `shouldBe` "test"
          fakeAccountEnvironments <- selectFakeAccountEnvironments accountId connection
          let expectedFakeAccountEnvironment =
                FakeAccountEnvironment { _fakeAccountEnvironmentAccountId = accountId
                                       , _fakeAccountEnvironmentEnvironmentId = environmentId
                                       }
          fakeAccountEnvironments `shouldSatisfy` (elem expectedFakeAccountEnvironment)
