{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

module ScenarioFolder.AppSpec where

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
import           ScenarioCollection.DB
import           ScenarioCollection.Model
import           ScenarioNode.DB
import           ScenarioNode.Model


-- * client


createScenarioFolder :: Auth.Token -> UUID -> NewScenarioFolder -> ClientM ()
createRootScenarioFolder :: Auth.Token -> UUID -> NewRootScenarioFolder -> ClientM ()
createScenarioFolder :<|> createRootScenarioFolder =
  client (Proxy :: Proxy (PScenarioFolderApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $ do


-- ** create scenario folder


    describe "create a scenario folder" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          accountId <- insertFakeAccount defaultNewFakeAccount1 connection
          token <- signedUserToken accountId
          let newScenarioFolder = mkNewScenarioFolder UUID.nil UUID.nil
          try clientEnv (createScenarioFolder token UUID.nil newScenarioFolder) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node parent doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          accountId <- insertFakeAccount defaultNewFakeAccount1 connection
          ScenarioCollection scenarioCollectionId _ <- insertSampleScenarioCollection accountId connection
          token <- signedUserToken accountId
          let newScenarioFolder = mkNewScenarioFolder UUID.nil UUID.nil
          try clientEnv (createScenarioFolder token scenarioCollectionId newScenarioFolder) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node parent exist but isn't a scenario folder" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          accountId <- insertFakeAccount defaultNewFakeAccount1 connection
          ScenarioCollection scenarioCollectionId scenarioNodes <- insertSampleScenarioCollection accountId connection
          let fileId = Maybe.fromJust (getFirstScenarioFile scenarioNodes) ^. scenarioNodeId
          token <- signedUserToken accountId
          let newScenarioFolder = mkNewScenarioFolder UUID.nil fileId
          try clientEnv (createScenarioFolder token scenarioCollectionId newScenarioFolder) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the scenario folder" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          accountId <- insertFakeAccount defaultNewFakeAccount1 connection
          token <- signedUserToken accountId
          ScenarioCollection scenarioCollectionId scenarioNodes <- insertSampleScenarioCollection accountId connection
          let folderId = Maybe.fromJust (getFirstScenarioFolder scenarioNodes) ^. scenarioNodeId
          let newScenarioFolder = mkNewScenarioFolder UUID.nil folderId
          _ <- try clientEnv (createScenarioFolder token scenarioCollectionId newScenarioFolder)
          fakeScenarioFolder <- selectFakeScenarioFolder UUID.nil connection
          fakeScenarioFolder `shouldBe`  FakeScenarioFolder { _fakeScenarioFolderParentId   = Just folderId
                                                            , _fakeScenarioFolderName       = "whatever"
                                                            }


-- ** create root scenario folder


    describe "create a root scenario folder" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          accountId <- insertFakeAccount defaultNewFakeAccount1 connection
          token <- signedUserToken accountId
          let newRootScenarioFolder = mkNewRootScenarioFolder UUID.nil
          try clientEnv (createRootScenarioFolder token UUID.nil newRootScenarioFolder) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the scenario folder" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          accountId <- insertFakeAccount defaultNewFakeAccount1 connection
          token <- signedUserToken accountId
          scenarioCollectionId <- insertFakeScenarioCollection accountId connection
          let newRootScenarioFolder = mkNewRootScenarioFolder UUID.nil
          _ <- try clientEnv (createRootScenarioFolder token scenarioCollectionId newRootScenarioFolder)
          fakeScenarioFolder <- selectFakeScenarioFolder UUID.nil connection
          fakeScenarioFolder `shouldBe`  FakeScenarioFolder { _fakeScenarioFolderParentId = Nothing
                                                            , _fakeScenarioFolderName       = "new folder"
                                                            }

  where
    mkNewScenarioFolder :: UUID -> UUID -> NewScenarioFolder
    mkNewScenarioFolder id parentId =
      NewScenarioFolder { _newScenarioFolderId           = id
                        , _newScenarioFolderParentNodeId = parentId
                        , _newScenarioFolderName         = "whatever"
                        }

    mkNewRootScenarioFolder :: UUID -> NewRootScenarioFolder
    mkNewRootScenarioFolder id =
      NewRootScenarioFolder { _newRootScenarioFolderId = id }
