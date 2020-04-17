{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators             #-}


module ScenarioNode.AppSpec where

import           Control.Lens.Getter      ((^.))
import qualified Data.Maybe               as Maybe
import           Data.UUID
import qualified Data.UUID                as UUID
import qualified Network.HTTP.Types       as HTTP
import           Servant
import qualified Servant.Auth.Client      as Auth
import qualified Servant.Auth.Server      as Auth
import           Servant.Client           (ClientM, client)
import           Test.Hspec

import           App
import           DBUtil
import           Helper.App
import           ScenarioCollection.Model
import           ScenarioNode.Model


-- * client


updateScenarioNodeHandler :: Auth.Token -> UUID -> UUID -> UpdateScenarioNode -> ClientM ()
deleteScenarioNodeHandler :: Auth.Token -> UUID -> UUID -> ClientM ()
updateScenarioNodeHandler :<|> deleteScenarioNodeHandler =
  client (Proxy :: Proxy (ScenarioNodeApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $ do


-- ** update scenario node


    describe "update scenario node" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (updateScenarioNodeHandler token UUID.nil UUID.nil updateScenarioNode) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node doesnt exist" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId _) <- insertSampleScenarioCollection accountId connection
          try clientEnv (updateScenarioNodeHandler token scenarioCollectionId UUID.nil updateScenarioNode) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 if the scenario node doesnt belong to the account" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, token } -> do
          accountId2 <- insertFakeAccount defaultNewFakeAccount2 connection
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId2 connection
          let nodeId = head scenarioNodes ^. scenarioNodeId
          try clientEnv (updateScenarioNodeHandler token scenarioCollectionId nodeId updateScenarioNode) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "modifies a scenario folder" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let nodeId = head scenarioNodes ^. scenarioNodeId
          _ <- try clientEnv (updateScenarioNodeHandler token scenarioCollectionId nodeId updateScenarioNode)
          FakeScenarioFolder { _fakeScenarioFolderName } <- selectFakeScenarioFolder nodeId connection
          _fakeScenarioFolderName `shouldBe` "newName"

      it "modifies a scenario file" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let nodeId = Maybe.fromJust (getFirstScenarioFile scenarioNodes) ^. scenarioNodeId
          _ <- try clientEnv (updateScenarioNodeHandler token scenarioCollectionId nodeId updateScenarioNode)
          FakeScenarioFile { _fakeScenarioFileName } <- selectFakeScenarioFile nodeId connection
          _fakeScenarioFileName `shouldBe` "newName"


-- ** delete scenario node


    describe "delete scenario node" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (deleteScenarioNodeHandler token UUID.nil UUID.nil) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node doesnt exist" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId _) <- insertSampleScenarioCollection accountId connection
          try clientEnv (deleteScenarioNodeHandler token scenarioCollectionId UUID.nil) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 if the scenario node doesnt belong to the account" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, token } -> do
          accountId2 <- insertFakeAccount defaultNewFakeAccount2 connection
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId2 connection
          let nodeId = head scenarioNodes ^. scenarioNodeId
          try clientEnv (deleteScenarioNodeHandler token scenarioCollectionId nodeId) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "delete a scenario node" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let nodeId = head scenarioNodes ^. scenarioNodeId
          selectScenarioNodeExists nodeId connection `shouldReturn` True
          _ <- try clientEnv (deleteScenarioNodeHandler token scenarioCollectionId nodeId)
          selectScenarioNodeExists nodeId connection `shouldReturn` False

  where
    updateScenarioNode :: UpdateScenarioNode
    updateScenarioNode =
      UpdateScenarioNode { _updateScenarioNodeName = "newName" }
