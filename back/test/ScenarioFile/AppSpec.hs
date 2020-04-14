{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

module ScenarioFile.AppSpec where

import           Control.Lens.Getter      ((^.))
import qualified Data.Maybe               as Maybe
import           Data.UUID
import qualified Data.UUID                as UUID
import qualified Network.HTTP.Types       as HTTP
import           Servant                  hiding (Header)
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


createScenarioFileHandler :: Auth.Token -> UUID -> NewScenarioFile -> ClientM ()
createRootScenarioFileHandler :: Auth.Token -> UUID -> NewRootScenarioFile -> ClientM ()
createScenarioFileHandler
  :<|> createRootScenarioFileHandler =
  client (Proxy :: Proxy (ScenarioFileApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $ do


-- ** create scenario file


    describe "create a scenario file" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } -> do
          let newScenarioFile = mkNewScenarioFile UUID.nil UUID.nil
          try clientEnv (createScenarioFileHandler token UUID.nil newScenarioFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node parent doesnt exist" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId _) <- insertSampleScenarioCollection accountId connection
          let newScenarioFile = mkNewScenarioFile UUID.nil UUID.nil
          try clientEnv (createScenarioFileHandler token scenarioCollectionId newScenarioFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node parent exist but isn't a scenario folder" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let fileId = Maybe.fromJust (getFirstScenarioFile scenarioNodes) ^. scenarioNodeId
          let newScenarioFile = mkNewScenarioFile UUID.nil fileId
          try clientEnv (createScenarioFileHandler token scenarioCollectionId newScenarioFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the scenario file" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let folderId = Maybe.fromJust (getFirstScenarioFolder scenarioNodes) ^. scenarioNodeId
          let newScenarioFile = mkNewScenarioFile UUID.nil folderId
          _ <- try clientEnv (createScenarioFileHandler token scenarioCollectionId newScenarioFile)
          fakeScenarioFile <- selectFakeScenarioFile UUID.nil connection
          fakeScenarioFile `shouldBe`  FakeScenarioFile { _fakeScenarioFileParentId    = Just folderId
                                                        , _fakeScenarioFileName        = "new scenario"
                                                        , _fakeScenarioFileSceneNodeId = Nothing
                                                        }

-- ** create root scenario file


    describe "create a root scenario file" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } -> do
          let newRootScenarioFile = mkNewRootScenarioFile UUID.nil
          try clientEnv (createRootScenarioFileHandler token UUID.nil newRootScenarioFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the scenario file" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          scenarioCollectionId <- insertFakeScenarioCollection accountId connection
          let newRootScenarioFile = mkNewRootScenarioFile UUID.nil
          _ <- try clientEnv (createRootScenarioFileHandler token scenarioCollectionId newRootScenarioFile)
          fakeScenarioFile <- selectFakeScenarioFile UUID.nil connection
          fakeScenarioFile `shouldBe` FakeScenarioFile { _fakeScenarioFileParentId = Nothing
                                                       , _fakeScenarioFileName = "new scenario"
                                                       , _fakeScenarioFileSceneNodeId = Nothing
                                                       }



  where
    mkNewScenarioFile :: UUID -> UUID -> NewScenarioFile
    mkNewScenarioFile id parentId =
      NewScenarioFile { _newScenarioFileId           = id
                      , _newScenarioFileParentNodeId = parentId
                      , _newScenarioFileName = "new scenario"
                      }

    mkNewRootScenarioFile :: UUID -> NewRootScenarioFile
    mkNewRootScenarioFile id =
      NewRootScenarioFile { _newRootScenarioFileId = id
                          , _newRootScenarioFileName = "new scenario"
                          }
