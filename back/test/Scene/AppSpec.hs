{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

module Scene.AppSpec where

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

import           Account.DB
import           App
import           Helper.App
import           RequestCollection.DB
import           RequestCollection.Model
import           RequestNode.DB
import           RequestNode.Model
import           ScenarioCollection.DB
import           ScenarioCollection.Model
import           ScenarioNode.DB
import           ScenarioNode.Model

-- * client


createSceneHandler :: Auth.Token -> UUID -> NewScene -> ClientM ()
createSceneHandler =
  client (Proxy :: Proxy (PSceneApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $


-- ** create scenario file


    describe "create a scene" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } -> do
          let newScene = mkNewScene UUID.nil UUID.nil UUID.nil
          try clientEnv (createSceneHandler token UUID.nil newScene) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node doesnt exist" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          ScenarioCollection _ _ <- insertSampleScenarioCollection accountId connection
          let newScene = mkNewScene UUID.nil UUID.nil UUID.nil
          try clientEnv (createSceneHandler token UUID.nil newScene) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node exists but isn't a scenario file" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          ScenarioCollection _ scenarioNodes <- insertSampleScenarioCollection accountId connection
          let folderId = Maybe.fromJust (getFirstScenarioFolder scenarioNodes) ^. scenarioNodeId
          let newScene = mkNewScene UUID.nil UUID.nil UUID.nil
          try clientEnv (createSceneHandler token folderId newScene) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node exists but isn't a scenario folder" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          ScenarioCollection _ scenarioNodes <- insertSampleScenarioCollection accountId connection
          let fileId = Maybe.fromJust (getFirstScenarioFile scenarioNodes) ^. scenarioNodeId
          let newScene = mkNewScene UUID.nil UUID.nil UUID.nil
          try clientEnv (createSceneHandler token fileId newScene) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 if the related request file doesnt belong to the account" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          (accountId2, _) <- withAccountAndToken defaultNewFakeAccount2 connection
          ScenarioCollection _ scenarioNodes <- insertSampleScenarioCollection accountId connection
          RequestCollection _ requestNodes <- insertSampleRequestCollection accountId2 connection
          let requestFileId = Maybe.fromJust (getFirstFile requestNodes) ^. requestNodeId
          let scenarioFile = Maybe.fromJust (getFirstScenarioFile scenarioNodes)
          let scenarioFileId = scenarioFile ^. scenarioNodeId
          let scenarioFirstScene = head $ scenarioFile ^. scenarioNodeScenes
          let newScene = mkNewScene UUID.nil (scenarioFirstScene ^. sceneId) requestFileId
          try clientEnv (createSceneHandler token scenarioFileId newScene) `shouldThrow` errorsWithStatus HTTP.notFound404


  where
    mkNewScene :: UUID -> UUID -> UUID -> NewScene
    mkNewScene id parentId requestFileId =
      NewScene { _newSceneId = id
               , _newSceneSceneNodeParentId = parentId
               , _newSceneRequestFileNodeId = requestFileId
               }
