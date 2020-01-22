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
          (accountId, token) <- withAccountAndToken defaultNewFakeAccount1 connection
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
          (accountId1, token) <- withAccountAndToken defaultNewFakeAccount1 connection
          (accountId2, _) <- withAccountAndToken defaultNewFakeAccount2 connection
          environmentId1 <- insertNewFakeEnvironment (newFakeEnvironment accountId1 "test1") connection
          _ <- insertNewFakeEnvironment (newFakeEnvironment accountId2 "test2") connection

          let newFakeKeyValues =
                [ NewFakeKeyValue { _fakeKeyValueEnvironmentId = environmentId1
                                  , _fakeKeyValueKey = "1k"
                                  , _fakeKeyValueValue = "1v"
                                  }
                , NewFakeKeyValue { _fakeKeyValueEnvironmentId = environmentId1
                                  , _fakeKeyValueKey = "2k"
                                  , _fakeKeyValueValue = "2v"
                                  }
                ]
          [keyValue1, keyValue2] <- mapM (`insertNewFakeKeyValue` connection) newFakeKeyValues
          environments <- try clientEnv (getEnvironments token)
          let expectedEnvironments = [ Environment { _environmentId = environmentId1
                                                   , _environmentName = "test1"
                                                   , _environmentKeyValues = [ keyValue1, keyValue2 ]
                                                   }
                                     ]
          environments `shouldBe` expectedEnvironments


-- ** update environments


    let updateEnvironmentPayload = UpdateEnvironment { _name = "test2" }

    describe "update environment" $ do
      it "return 404 if environment doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (_, token) <- withAccountAndToken defaultNewFakeAccount1 connection
          try clientEnv (updateEnvironment token 1 updateEnvironmentPayload) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "return 404 when environment doesnt belong to account" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (_, token) <- withAccountAndToken defaultNewFakeAccount1 connection
          (accountId2, _) <- withAccountAndToken defaultNewFakeAccount2 connection
          environmentId <- insertNewFakeEnvironment (newFakeEnvironment accountId2 "test1") connection
          try clientEnv (updateEnvironment token environmentId updateEnvironmentPayload) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "should update environment" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, token) <- withAccountAndToken defaultNewFakeAccount1 connection
          environmentId <- insertNewFakeEnvironment (newFakeEnvironment accountId "test") connection
          _ <- try clientEnv (updateEnvironment token environmentId updateEnvironmentPayload)
          Just FakeEnvironment { _fakeEnvironmentName } <- selectFakeEnvironment environmentId connection
          _fakeEnvironmentName `shouldBe` "test2"


-- ** delete environments


    describe "delete environment" $ do
      it "return 404 if environment doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (_, token) <- withAccountAndToken defaultNewFakeAccount1 connection
          try clientEnv (deleteEnvironment token 1) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "return 404 if environment doesnt belong to account" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (_, token) <- withAccountAndToken defaultNewFakeAccount1 connection
          (accountId2, _) <- withAccountAndToken defaultNewFakeAccount2 connection
          let newFakeEnvironment =
                NewFakeEnvironment { _newFakeEnvironmentAccountId = accountId2
                                   , _newFakeEnvironmentName = "test"
                                   }
          environmentId <- insertNewFakeEnvironment newFakeEnvironment connection
          try clientEnv (deleteEnvironment token environmentId) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "deletes environment" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (accountId, token) <- withAccountAndToken defaultNewFakeAccount1 connection
          let newFakeEnvironment =
                NewFakeEnvironment { _newFakeEnvironmentAccountId = accountId
                                   , _newFakeEnvironmentName = "test"
                                   }
          environmentId <- insertNewFakeEnvironment newFakeEnvironment connection
          _ <- try clientEnv (deleteEnvironment token environmentId)
          mEnvironment <- selectFakeEnvironment environmentId connection
          mEnvironment `shouldSatisfy` Maybe.isNothing


-- ** update key values

    describe "update key values" $ do
      it "return 404 if environment doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          (_, token) <- withAccountAndToken defaultNewFakeAccount1 connection
          try clientEnv (updateKeyValues token 1 []) `shouldThrow` errorsWithStatus HTTP.notFound404


  where
    withAccountAndToken :: NewFakeAccount -> PG.Connection -> IO (Int, Auth.Token)
    withAccountAndToken newFakeAccount connection = do
      (accountId, _) <- insertFakeAccount newFakeAccount connection
      token <- signedUserToken accountId
      return (accountId, token)

    newEnvironment :: NewEnvironment = NewEnvironment { _newEnvironmentName = "test" }

    newFakeEnvironment :: Int -> String -> NewFakeEnvironment
    newFakeEnvironment accountId name =
      NewFakeEnvironment { _newFakeEnvironmentAccountId = accountId
                         , _newFakeEnvironmentName = name
                         }
