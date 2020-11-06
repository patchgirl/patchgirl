{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}



module ScenarioNode.AppSpec where

import           Data.Function                          ((&))
import qualified Data.Maybe                             as Maybe
import qualified Network.HTTP.Types                     as HTTP
import           Servant
import qualified Servant.Auth.Client                    as Auth
import qualified Servant.Auth.Server                    as Auth
import           Servant.Client                         (ClientM, client)
import           Test.Hspec

import           DBUtil
import           Helper.App
import           PatchGirl.Web.Api
import           PatchGirl.Web.Id
import           PatchGirl.Web.ScenarioCollection.Model
import           PatchGirl.Web.ScenarioNode.Model
import           PatchGirl.Web.Server


-- * client


updateScenarioNodeHandler :: Auth.Token -> Id ScenarioCol -> Id Scenario -> UpdateScenarioNode -> ClientM ()
deleteScenarioNodeHandler :: Auth.Token -> Id ScenarioCol -> Id Scenario -> ClientM ()
updateScenarioNodeHandler :<|> deleteScenarioNodeHandler =
  client (Proxy :: Proxy (ScenarioNodeApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultEnv) $ do


-- ** update scenario node


    describe "update scenario node" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } ->
          try clientEnv (updateScenarioNodeHandler token nilId nilId updateScenarioNode) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId _) <- insertSampleScenarioCollection accountId connection
          try clientEnv (updateScenarioNodeHandler token scenarioCollectionId nilId updateScenarioNode) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 if the scenario node doesnt belong to the account" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, token } -> do
          accountId2 <- insertFakeAccount defaultNewFakeAccount2 connection
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId2 connection
          let nodeId = head scenarioNodes & _scenarioNodeId
          try clientEnv (updateScenarioNodeHandler token scenarioCollectionId nodeId updateScenarioNode) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "modifies a scenario folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let nodeId = head scenarioNodes & _scenarioNodeId
          _ <- try clientEnv (updateScenarioNodeHandler token scenarioCollectionId nodeId updateScenarioNode)
          FakeScenarioFolder { _fakeScenarioFolderName } <- selectFakeScenarioFolder nodeId connection
          _fakeScenarioFolderName `shouldBe` "newName"

      it "modifies a scenario file" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let nodeId = Maybe.fromJust (getFirstScenarioFile scenarioNodes) & _scenarioNodeId
          _ <- try clientEnv (updateScenarioNodeHandler token scenarioCollectionId nodeId updateScenarioNode)
          FakeScenarioFile { _fakeScenarioFileName } <- selectFakeScenarioFile nodeId connection
          _fakeScenarioFileName `shouldBe` "newName"


-- ** delete scenario node


    describe "delete scenario node" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } ->
          try clientEnv (deleteScenarioNodeHandler token nilId nilId) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId _) <- insertSampleScenarioCollection accountId connection
          try clientEnv (deleteScenarioNodeHandler token scenarioCollectionId nilId) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 if the scenario node doesnt belong to the account" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, token } -> do
          accountId2 <- insertFakeAccount defaultNewFakeAccount2 connection
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId2 connection
          let nodeId = head scenarioNodes & _scenarioNodeId
          try clientEnv (deleteScenarioNodeHandler token scenarioCollectionId nodeId) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "delete a scenario node" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let nodeId = head scenarioNodes & _scenarioNodeId
          selectScenarioNodeExists nodeId connection `shouldReturn` True
          _ <- try clientEnv (deleteScenarioNodeHandler token scenarioCollectionId nodeId)
          selectScenarioNodeExists nodeId connection `shouldReturn` False

  where
    updateScenarioNode :: UpdateScenarioNode
    updateScenarioNode =
      UpdateScenarioNode { _updateScenarioNodeName = "newName" }
