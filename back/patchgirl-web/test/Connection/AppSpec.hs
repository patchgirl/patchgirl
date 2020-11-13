{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Connection.AppSpec where

import           Data.Function                  ((&))
import qualified Data.Maybe                     as Maybe
import qualified Data.UUID.V4                   as UUID
import qualified Network.HTTP.Types             as HTTP
import           Servant
import qualified Servant.Auth.Client            as Auth
import qualified Servant.Auth.Server            as Auth
import           Servant.Client                 (ClientM, client)
import           Test.Hspec

import           DBUtil
import           Helper.App
import           PatchGirl.Web.Api
import           PatchGirl.Web.Connection.Model
import           PatchGirl.Web.Id
import           PatchGirl.Web.Server


-- * client


createConnection :: Auth.Token -> NewConnection -> ClientM ()
getConnections :: Auth.Token -> ClientM [Connection]
updateConnection :: Auth.Token -> Id Con -> UpdateConnection -> ClientM ()
deleteConnection :: Auth.Token -> Id Con -> ClientM ()
createConnection
  :<|> getConnections
  :<|> updateConnection
  :<|> deleteConnection =
  client (Proxy :: Proxy (ConnectionApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultEnv) $ do


-- ** create connection


    describe "create connection" $
      it "should create an connection and bind it to an account" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          newConnection <- mkNewConnection
          try clientEnv (createConnection token newConnection)
          fakeConnection :: Maybe FakeConnection <- selectFakeConnection (newConnection & _newConnectionId) connection
          let expected = Just $ FakeConnection { _fakeConnectionId = newConnection & _newConnectionId
                                               , _fakeConnectionAccountId = accountId
                                               , _fakeConnectionName = newConnection & _newConnectionName
                                               , _fakeConnectionTag = newConnection & _newConnectionTag
                                               , _fakeConnectionPgHost     = newConnection & _newConnectionPgHost
                                               , _fakeConnectionPgPassword = newConnection & _newConnectionPgPassword
                                               , _fakeConnectionPgPort     = newConnection & _newConnectionPgPort
                                               , _fakeConnectionPgUser     = newConnection & _newConnectionPgUser
                                               , _fakeConnectionPgDbName   = newConnection & _newConnectionPgDbName
                                               }
          fakeConnection `shouldBe` expected


-- ** get connections


    describe "get connections" $
      it "should get connections bound to the account" $ \clientEnv -> do
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          connection1 <- mkNewFakeConnection
          connection2 <- mkNewFakeConnection
          (accountId2, _) <- withAccountAndToken defaultNewFakeAccount2 connection
          _ <- insertNewFakeConnection connection1 accountId connection
          _ <- insertNewFakeConnection connection2 accountId2 connection
          connections <- try clientEnv (getConnections token)
          let expectedConnections = [ Connection { _connectionId = connection1 & _newFakeConnectionId
                                                 , _connectionName = "name"
                                                 , _connectionType = PgTag
                                                 , _connectionDbHost = "host"
                                                 , _connectionDbPassword = "password"
                                                 , _connectionDbPort = "port"
                                                 , _connectionDbUser = "user"
                                                 , _connectionDbName = "dbname"
                                                 }
                                    ]
          connections `shouldBe` expectedConnections


-- ** update connections


    let updateConnectionPayload = UpdateConnection { _updateConnectionName = "test2" }

    describe "update connection" $ do
      it "return 404 if connection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } -> do
          try clientEnv (updateConnection token nilId updateConnectionPayload) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "return 404 when connection doesnt belong to account" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, token } -> do
          connection1 <- mkNewFakeConnection
          (accountId2, _) <- withAccountAndToken defaultNewFakeAccount2 connection
          _ <- insertNewFakeConnection connection1 accountId2 connection
          try clientEnv (updateConnection token (connection1 & _newFakeConnectionId) updateConnectionPayload) `shouldThrow` errorsWithStatus HTTP.notFound404


      it "should update connection" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          connection1 <- mkNewFakeConnection
          _ <- insertNewFakeConnection connection1 accountId connection
          _ <- try clientEnv (updateConnection token (connection1 & _newFakeConnectionId) updateConnectionPayload)
          Just FakeConnection { _fakeConnectionName } <- selectFakeConnection (connection1 & _newFakeConnectionId) connection
          _fakeConnectionName `shouldBe` "test2"


-- ** delete connections


    describe "delete connection" $ do
      it "return 404 if environment doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } ->
          try clientEnv (deleteConnection token nilId) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "return 404 if connection doesnt exist" $ \clientEnv -> do
        cleanDBAndCreateAccount $ \Test { token, connection } -> do
          connection1 <- mkNewFakeConnection
          (accountId2, _) <- withAccountAndToken defaultNewFakeAccount2 connection
          _ <- insertNewFakeConnection connection1 accountId2 connection
          try clientEnv (deleteConnection token (connection1 & _newFakeConnectionId)) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "deletes connection" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          connection1 <- mkNewFakeConnection
          _ <- insertNewFakeConnection connection1 accountId connection
          try clientEnv (deleteConnection token (connection1 & _newFakeConnectionId))
          mConnection <- selectFakeConnection (connection1 & _newFakeConnectionId) connection
          mConnection `shouldSatisfy` Maybe.isNothing

  where
    mkNewConnection :: IO NewConnection
    mkNewConnection = do
      id <- UUID.nextRandom
      return $ NewConnection
        { _newConnectionId = Id id
        , _newConnectionName = "name"
        , _newConnectionTag = PgTag
        , _newConnectionPgHost = ""
        , _newConnectionPgPassword = ""
        , _newConnectionPgPort     = ""
        , _newConnectionPgUser     = ""
        , _newConnectionPgDbName   = ""
        }
