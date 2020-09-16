{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Environment.AppSpec where

import qualified Data.Maybe                 as Maybe
import           Data.UUID                  (UUID)
import qualified Data.UUID.V4                         as UUID
import qualified Data.UUID                         as UUID
import qualified Database.PostgreSQL.Simple as PG
import qualified Network.HTTP.Types         as HTTP
import           Servant
import qualified Servant.Auth.Client        as Auth
import qualified Servant.Auth.Server        as Auth
import           Servant.Client             (ClientM, client)
import           Test.Hspec

import           DBUtil
import           Helper.App
import           PatchGirl.Web.Server
import           PatchGirl.Web.Environment.Model
import           PatchGirl.Web.Api


-- * client


createEnvironment :: Auth.Token -> NewEnvironment -> ClientM ()
getEnvironments :: Auth.Token -> ClientM [Environment]
updateEnvironment :: Auth.Token -> UUID -> UpdateEnvironment -> ClientM ()
deleteEnvironment :: Auth.Token -> UUID -> ClientM ()
updateKeyValues :: Auth.Token -> UUID -> [NewKeyValue] -> ClientM ()
deleteKeyValue :: Auth.Token -> UUID -> UUID -> ClientM ()
createEnvironment
  :<|> getEnvironments
  :<|> updateEnvironment
  :<|> deleteEnvironment
  :<|> updateKeyValues
  :<|> deleteKeyValue =
  client (Proxy :: Proxy (EnvironmentApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultEnv) $ do


-- ** create environment


    describe "create environment" $
      it "should create an environment and bind it to an account" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          newEnvironment <- mkNewEnvironment
          try clientEnv (createEnvironment token newEnvironment)
          Just FakeEnvironment { _fakeEnvironmentName } <- selectFakeEnvironment (_newEnvironmentId newEnvironment) connection
          _fakeEnvironmentName `shouldBe` "test"
          fakeAccountEnvironments <- selectFakeAccountEnvironments accountId connection
          let expectedFakeAccountEnvironment =
                FakeAccountEnvironment { _fakeAccountEnvironmentAccountId = accountId
                                       , _fakeAccountEnvironmentEnvironmentId = _newEnvironmentId newEnvironment
                                       }
          fakeAccountEnvironments `shouldSatisfy` elem expectedFakeAccountEnvironment


-- ** get environments


    describe "get environments" $
      it "should get environments bound to the account" $ \clientEnv -> do
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do

          (accountId2, _) <- withAccountAndToken defaultNewFakeAccount2 connection
          environmentId1 <- insertNewFakeEnvironment (newFakeEnvironment accountId "test1") connection
          _ <- insertNewFakeEnvironment (newFakeEnvironment accountId2 "test2") connection

          let newFakeKeyValues =
                [ NewFakeKeyValue { _newFakeKeyValueEnvironmentId = environmentId1
                                  , _newFakeKeyValueKey = "1k"
                                  , _newFakeKeyValueValue = "1v"
                                  , _newFakeKeyValueHidden = False
                                  }
                , NewFakeKeyValue { _newFakeKeyValueEnvironmentId = environmentId1
                                  , _newFakeKeyValueKey = "2k"
                                  , _newFakeKeyValueValue = "2v"
                                  , _newFakeKeyValueHidden = True
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


    let updateEnvironmentPayload = UpdateEnvironment { _updateEnvironmentName = "test2" }

    describe "update environment" $ do
      it "return 404 if environment doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } ->
          try clientEnv (updateEnvironment token UUID.nil updateEnvironmentPayload) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "return 404 when environment doesnt belong to account" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, token } -> do
          (accountId2, _) <- withAccountAndToken defaultNewFakeAccount2 connection
          environmentId <- insertNewFakeEnvironment (newFakeEnvironment accountId2 "test1") connection
          try clientEnv (updateEnvironment token environmentId updateEnvironmentPayload) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "should update environment" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          environmentId <- insertNewFakeEnvironment (newFakeEnvironment accountId "test") connection
          _ <- try clientEnv (updateEnvironment token environmentId updateEnvironmentPayload)
          Just FakeEnvironment { _fakeEnvironmentName } <- selectFakeEnvironment environmentId connection
          _fakeEnvironmentName `shouldBe` "test2"


-- ** delete environments


    describe "delete environment" $ do
      it "return 404 if environment doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } ->
          try clientEnv (deleteEnvironment token UUID.nil) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "return 404 if environment doesnt belong to account" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, token } -> do
          environmentId <- environmentThatDoesntBelongToAccountToken connection
          try clientEnv (deleteEnvironment token environmentId) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "deletes environment" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          environmentId <- insertNewFakeEnvironment (newFakeEnvironment accountId "test") connection
          _ <- try clientEnv (deleteEnvironment token environmentId)
          mEnvironment <- selectFakeEnvironment environmentId connection
          mEnvironment `shouldSatisfy` Maybe.isNothing


-- ** update key values


    describe "update key values" $ do
      it "return 404 if environment doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } ->
          try clientEnv (updateKeyValues token UUID.nil []) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "return 404 if environment doesnt belong to account" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, token } -> do
          environmentId <- environmentThatDoesntBelongToAccountToken connection
          try clientEnv (updateKeyValues token environmentId []) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "updates the environment key values" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          environmentId <- insertNewFakeEnvironment (newFakeEnvironment accountId "test") connection
          try clientEnv (updateKeyValues token environmentId [])
          selectFakeKeyValues environmentId connection `shouldReturn` []

          let newKeyValues =
                [ NewKeyValue { _newKeyValueId = UUID.nil
                              , _newKeyValueKey = "key1"
                              , _newKeyValueValue = "value1"
                              , _newKeyValueHidden = True
                              }
                ]
          try clientEnv (updateKeyValues token environmentId newKeyValues)
          selectFakeKeyValues environmentId connection `shouldReturn` [ KeyValue { _keyValueId = UUID.nil
                                                                                 , _keyValueKey = "key1"
                                                                                 , _keyValueValue = "value1"
                                                                                 , _keyValueHidden = True
                                                                                 }
                                                                      ]


-- ** delete key values


    describe "delete key values" $ do
      it "return 404 if environment doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } ->
          try clientEnv (deleteKeyValue token UUID.nil UUID.nil) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "return 404 if environment doesnt belong to account" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, token } -> do
          environmentId <- environmentThatDoesntBelongToAccountToken connection
          try clientEnv (deleteKeyValue token environmentId UUID.nil) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "return 404 if the key value doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          environmentId <- insertNewFakeEnvironment (newFakeEnvironment accountId "test") connection
          try clientEnv (deleteKeyValue token environmentId UUID.nil) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "deletes the key value" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          environmentId <- insertNewFakeEnvironment (newFakeEnvironment accountId "test") connection
          KeyValue { _keyValueId } <-
            insertNewFakeKeyValue NewFakeKeyValue { _newFakeKeyValueEnvironmentId = environmentId
                                                  , _newFakeKeyValueKey = "key1"
                                                  , _newFakeKeyValueValue = "value1"
                                                  , _newFakeKeyValueHidden = True
                                                  } connection
          try clientEnv (deleteKeyValue token environmentId _keyValueId )
          selectFakeKeyValues _keyValueId connection `shouldReturn` []


  where
    mkNewEnvironment :: IO NewEnvironment
    mkNewEnvironment = do
      id <- UUID.nextRandom
      return $ NewEnvironment { _newEnvironmentId = id
                              , _newEnvironmentName = "test"
                              }

    newFakeEnvironment :: UUID -> String -> NewFakeEnvironment
    newFakeEnvironment accountId name =
      NewFakeEnvironment { _newFakeEnvironmentAccountId = accountId
                         , _newFakeEnvironmentName = name
                         }

    environmentThatDoesntBelongToAccountToken :: PG.Connection -> IO UUID
    environmentThatDoesntBelongToAccountToken connection = do
      (accountId2, _) <- withAccountAndToken defaultNewFakeAccount2 connection
      insertNewFakeEnvironment (newFakeEnvironment accountId2 "test") connection
