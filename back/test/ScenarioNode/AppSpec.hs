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

import           Account.DB
import           App
import           Helper.App
import           RequestCollection.DB
import           ScenarioCollection.DB
import           ScenarioCollection.Model
import           ScenarioNode.DB
import           ScenarioNode.Model


-- * client


updateScenarioNodeHandler :: Auth.Token -> UUID -> UUID -> UpdateScenarioNode -> ClientM ()
--deleteScenarioNodeHandler :: Auth.Token -> UUID -> UUID -> ClientM ()
updateScenarioNodeHandler = -- :<|> deleteScenarioNodeHandler =
  client (Proxy :: Proxy (PScenarioNodeApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $ do


-- ** update scenario node


    describe "update scenario node" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          accountId <- insertFakeAccount defaultNewFakeAccount1 connection
          token <- signedUserToken accountId
          try clientEnv (updateScenarioNodeHandler token UUID.nil UUID.nil updateScenarioNode) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          accountId <- insertFakeAccount defaultNewFakeAccount1 connection
          token <- signedUserToken accountId
          ScenarioCollection scenarioCollectionId scenarioNodes <- insertSampleScenarioCollection accountId connection
          try clientEnv (updateScenarioNodeHandler token scenarioCollectionId UUID.nil updateScenarioNode) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 if the scenario node doesnt belong to the account" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          accountId2 <- insertFakeAccount defaultNewFakeAccount2 connection
          accountId1 <- insertFakeAccount defaultNewFakeAccount1 connection
          ScenarioCollection scenarioCollectionId scenarioNodes <- insertSampleScenarioCollection accountId2 connection
          let nodeId = head scenarioNodes ^. scenarioNodeId
          token <- signedUserToken accountId1
          try clientEnv (updateScenarioNodeHandler token scenarioCollectionId nodeId updateScenarioNode) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "modifies a scenario folder" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          accountId <- insertFakeAccount defaultNewFakeAccount1 connection
          ScenarioCollection scenarioCollectionId scenarioNodes <- insertSampleScenarioCollection accountId connection
          let nodeId = head scenarioNodes ^. scenarioNodeId
          token <- signedUserToken accountId
          _ <- try clientEnv (updateScenarioNodeHandler token scenarioCollectionId nodeId updateScenarioNode)
          FakeScenarioFolder { _fakeScenarioFolderName } <- selectFakeScenarioFolder nodeId connection
          _fakeScenarioFolderName `shouldBe` "newName"

      it "modifies a scenario file" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          accountId <- insertFakeAccount defaultNewFakeAccount1 connection
          ScenarioCollection scenarioCollectionId scenarioNodes <- insertSampleScenarioCollection accountId connection
          let nodeId = (Maybe.fromJust $ getFirstScenarioFile scenarioNodes) ^. scenarioNodeId
          token <- signedUserToken accountId
          _ <- try clientEnv (updateScenarioNodeHandler token scenarioCollectionId nodeId updateScenarioNode)
          FakeScenarioFile { _fakeScenarioFileName } <- selectFakeScenarioFile nodeId connection
          _fakeScenarioFileName `shouldBe` "newName"

  where
    updateScenarioNode :: UpdateScenarioNode
    updateScenarioNode =
      UpdateScenarioNode { _updateScenarioNodeName = "newName" }
