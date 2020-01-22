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

import           Account.DB
import           App
import qualified Data.Maybe                 as Maybe
import qualified Database.PostgreSQL.Simple as PG
import           Environment.App
import           Environment.DB
import           Helper.App
import qualified Network.HTTP.Types         as HTTP
import           Servant
import qualified Servant.Auth.Client        as Auth
import qualified Servant.Auth.Server        as Auth
import           Servant.Client             (ClientM, client)
import           Test.Hspec

-- * client


createEnvironment :: Auth.Token -> NewEnvironment -> ClientM Int
getEnvironments :: Auth.Token -> ClientM [Environment]
updateEnvironment :: Auth.Token -> Int -> UpdateEnvironment -> ClientM ()
deleteEnvironment :: Auth.Token -> Int -> ClientM ()
updateKeyValues :: Auth.Token -> Int -> [NewKeyValue] -> ClientM [KeyValue]
deleteKeyValue :: Auth.Token -> Int -> Int -> ClientM ()
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


    describe "create environment" $
      it "should create an environment and bind it to an account" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, token, newEnvironment) <- withAccountAndEnvironment connection
          environmentId <- try clientEnv (createEnvironment token newEnvironment)
          Just FakeEnvironment { _fakeEnvironmentName } <- selectFakeEnvironment environmentId connection
          _fakeEnvironmentName `shouldBe` "test"
          fakeAccountEnvironments <- selectFakeAccountEnvironments accountId connection
          let expectedFakeAccountEnvironment =
                FakeAccountEnvironment { _fakeAccountEnvironmentAccountId = accountId
                                       , _fakeAccountEnvironmentEnvironmentId = environmentId
                                       }
          fakeAccountEnvironments `shouldSatisfy` elem expectedFakeAccountEnvironment


-- ** get environments


    describe "get environments" $
      it "should get environments bound to the account" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (_, token, newEnvironment) <- withAccountAndEnvironment connection
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

    let updateEnvironmentPayload = UpdateEnvironment { _name = "test2" }

    describe "update environment" $ do
      it "return 404 if environment doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (_, token, _) <- withAccountAndEnvironment connection
          try clientEnv (updateEnvironment token 1 updateEnvironmentPayload) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "return 404 when environment doesnt belong to account" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (_, token, newEnvironment) <- withAccountAndEnvironment connection
          environmentId <- try clientEnv (createEnvironment token newEnvironment)
          try clientEnv (updateEnvironment token (environmentId + 1) updateEnvironmentPayload) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "should update environment" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (_, token, newEnvironment) <- withAccountAndEnvironment connection
          environmentId <- try clientEnv (createEnvironment token newEnvironment)
          _ <- try clientEnv (updateEnvironment token environmentId updateEnvironmentPayload)
          Just FakeEnvironment { _fakeEnvironmentName } <- selectFakeEnvironment environmentId connection
          _fakeEnvironmentName `shouldBe` "test2"


-- ** delete environments


    describe "delete environment" $ do
      it "return 404 if environment doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (_, token, _) <- withAccountAndEnvironment connection
          try clientEnv (deleteEnvironment token 1) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "return 404 if environment doesnt belong to account" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId1, _) <- insertFakeAccount defaultNewFakeAccount1 connection
          (accountId2, _) <- insertFakeAccount defaultNewFakeAccount2 connection
          let newFakeEnvironment =
                NewFakeEnvironment { _newFakeEnvironmentAccountId = accountId2
                                   , _newFakeEnvironmentName = "test"
                                   }
          environmentId <- insertNewFakeEnvironment newFakeEnvironment connection
          token <- signedUserToken accountId1
          try clientEnv (deleteEnvironment token environmentId) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "deletes environment" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, token, _) <- withAccountAndEnvironment connection
          let newFakeEnvironment =
                NewFakeEnvironment { _newFakeEnvironmentAccountId = accountId
                                   , _newFakeEnvironmentName = "test"
                                   }
          environmentId <- insertNewFakeEnvironment newFakeEnvironment connection
          _ <- try clientEnv (deleteEnvironment token environmentId)
          mEnvironment <- selectFakeEnvironment environmentId connection
          mEnvironment `shouldSatisfy` Maybe.isNothing


  where
    withAccountAndEnvironment :: PG.Connection -> IO (Int, Auth.Token, NewEnvironment)
    withAccountAndEnvironment connection = do
      (accountId, _) <- insertFakeAccount defaultNewFakeAccount1 connection
      token <- signedUserToken accountId
      return (accountId, token, NewEnvironment { _newEnvironmentName = "test" })
