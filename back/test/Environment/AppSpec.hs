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
import qualified Network.HTTP.Types  as HTTP
import           Servant
import qualified Servant.Auth.Client as Auth
import qualified Servant.Auth.Server as Auth
import           Servant.Client      (ClientM, client)
import           Session.Model
import           Test.Hspec


-- * client


createEnvironment :: Auth.Token -> NewEnvironment -> ClientM Int
getEnvironments :: Auth.Token -> ClientM [Environment]
updateEnvironment :: Auth.Token -> Int -> UpdateEnvironment -> ClientM ()
createEnvironment
  :<|> getEnvironments
  :<|> updateEnvironment
  :<|> deleteEnvironment
  :<|> updateKeyValues
  :<|> deleteKeyValue =
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


-- ** get environments


    describe "get environments" $ do
      it "should get environments bound to the account" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount connection
          token <- signedUserToken accountId
          let newEnvironment = NewEnvironment { _newEnvironmentName = "test" }
          environmentId <- try clientEnv (createEnvironment token newEnvironment)
          let newKeyValues = [ NewKeyValue { _newKeyValueKey = "1k", _newKeyValueValue = "1v" }
                             , NewKeyValue { _newKeyValueKey = "2k", _newKeyValueValue = "2v" }
                             ]
          [keyValue1, keyValue2] <- try clientEnv (updateKeyValues token environmentId newKeyValues)
          environments <- try clientEnv (getEnvironments token)
          let expectedEnvironments = [ Environment { _environmentId = environmentId
                                                   , _environmentName = "test"
                                                   , _environmentKeyValues = [ keyValue1, keyValue2 ]
                                                   }
                                     ]
          environments `shouldBe` expectedEnvironments


-- ** update environments


    describe "update environment" $ do
      it "return 404 if environment doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount connection
          token <- signedUserToken accountId
          let updateEnvironmentPayload = UpdateEnvironment { _name = "whatever" }
          try clientEnv (updateEnvironment token 1 updateEnvironmentPayload) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "return 404 when environment doesnt belong to account" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount connection
          token <- signedUserToken accountId
          let newEnvironment = NewEnvironment { _newEnvironmentName = "test" }
          environmentId <- try clientEnv (createEnvironment token newEnvironment)
          let updateEnvironmentPayload = UpdateEnvironment { _name = "test2" }
          try clientEnv (updateEnvironment token (environmentId + 1) updateEnvironmentPayload) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "should update environment" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, _) <- insertFakeAccount defaultNewFakeAccount connection
          token <- signedUserToken accountId
          let newEnvironment = NewEnvironment { _newEnvironmentName = "test" }
          environmentId <- try clientEnv (createEnvironment token newEnvironment)
          let updateEnvironmentPayload = UpdateEnvironment { _name = "test2" }
          _ <- try clientEnv (updateEnvironment token environmentId updateEnvironmentPayload)
          FakeEnvironment { _fakeEnvironmentName } <- selectFakeEnvironment environmentId connection
          _fakeEnvironmentName `shouldBe` "test2"
