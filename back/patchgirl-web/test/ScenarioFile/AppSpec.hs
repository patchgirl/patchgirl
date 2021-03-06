{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module ScenarioFile.AppSpec where

import           Data.Function                          ((&))
import qualified Data.Maybe                             as Maybe
import qualified Database.PostgreSQL.Simple             as PG
import qualified Network.HTTP.Types                     as HTTP
import           Servant                                hiding (Header)
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


createScenarioFileHandler :: Auth.Token -> Id ScenarioCol -> NewScenarioFile -> ClientM ()
updateScenarioFileHandler :: Auth.Token -> Id ScenarioCol -> UpdateScenarioFile -> ClientM ()
createRootScenarioFileHandler :: Auth.Token -> Id ScenarioCol -> NewRootScenarioFile -> ClientM ()
createScenarioFileHandler
  :<|> updateScenarioFileHandler
  :<|> createRootScenarioFileHandler =
  client (Proxy :: Proxy (ScenarioFileApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultEnv) $ do


-- ** create scenario file


    describe "create a scenario file" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, token, accountId } -> do
          (_, newScenarioFile) <- mkNewScenarioFile nilId nilId accountId connection
          try clientEnv (createScenarioFileHandler token nilId newScenarioFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node parent doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId _) <- insertSampleScenarioCollection accountId connection
          (_, newScenarioFile) <- mkNewScenarioFile nilId nilId accountId connection
          try clientEnv (createScenarioFileHandler token scenarioCollectionId newScenarioFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node parent exist but isn't a scenario folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let fileId = Maybe.fromJust (getFirstScenarioFile scenarioNodes) & _scenarioNodeId
          (_, newScenarioFile) <- mkNewScenarioFile nilId fileId accountId connection
          try clientEnv (createScenarioFileHandler token scenarioCollectionId newScenarioFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the scenario file" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let folderId = Maybe.fromJust (getFirstScenarioFolder scenarioNodes) & _scenarioNodeId
          (environmentId, newScenarioFile) <- mkNewScenarioFile nilId folderId accountId connection
          _ <- try clientEnv (createScenarioFileHandler token scenarioCollectionId newScenarioFile)
          fakeScenarioFile <- selectFakeScenarioFile nilId connection
          fakeScenarioFile `shouldBe`  FakeScenarioFile { _fakeScenarioFileParentId    = Just folderId
                                                        , _fakeScenarioFileName        = "new scenario"
                                                        , _fakeScenarioFileSceneActorId = Nothing
                                                        , _fakeScenarioFileEnvironmentId = Just environmentId
                                                        }

-- ** update scenario file


    describe "update a scenario file" $ do
      it "returns 404 when scenario collection doesnt belong to account2" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, token } -> do
          (accountId2, _) <- withAccountAndToken defaultNewFakeAccount2 connection
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId2 connection
          let nodeId = Maybe.fromJust (getFirstScenarioFile scenarioNodes) & _scenarioNodeId
          let updateScenarioFile = mkUpdateScenarioFile nodeId nilId
          try clientEnv (updateScenarioFileHandler token scenarioCollectionId updateScenarioFile)
            `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario file doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, token, accountId } -> do
          (_, ScenarioCollection scenarioCollectionId _) <- insertSampleScenarioCollection accountId connection
          let updateScenarioFile = mkUpdateScenarioFile nilId nilId
          try clientEnv (updateScenarioFileHandler token scenarioCollectionId updateScenarioFile)
            `shouldThrow` errorsWithStatus HTTP.notFound404

      it "doesnt update when new environment doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, token, accountId } -> do
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let nodeId = Maybe.fromJust (getFirstScenarioFile scenarioNodes) & _scenarioNodeId
          let updateScenarioFile = mkUpdateScenarioFile nodeId nilId
          try clientEnv (updateScenarioFileHandler token scenarioCollectionId updateScenarioFile)
          fakeScenarioFile <- selectFakeScenarioFile nodeId connection
          _fakeScenarioFileEnvironmentId fakeScenarioFile `shouldBe` Nothing

      it "doesnt update when new environment doesnt belong to account" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, token, accountId } -> do
          (accountId2, _) <- withAccountAndToken defaultNewFakeAccount2 connection
          let newEnvironment = NewFakeEnvironment { _newFakeEnvironmentAccountId = accountId2
                                                  , _newFakeEnvironmentName      = "env"
                                                  }
          envId <- insertNewFakeEnvironment newEnvironment connection

          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let nodeId = Maybe.fromJust (getFirstScenarioFile scenarioNodes) & _scenarioNodeId
          let updateScenarioFile = mkUpdateScenarioFile nodeId envId
          try clientEnv (updateScenarioFileHandler token scenarioCollectionId updateScenarioFile)
          fakeScenarioFile <- selectFakeScenarioFile nodeId connection
          _fakeScenarioFileEnvironmentId fakeScenarioFile `shouldBe` Nothing


      it "update the environment" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, token, accountId } -> do
          let newEnvironment = NewFakeEnvironment { _newFakeEnvironmentAccountId = accountId
                                                  , _newFakeEnvironmentName      = "env"
                                                  }
          envId <- insertNewFakeEnvironment newEnvironment connection
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let nodeId = Maybe.fromJust (getFirstScenarioFile scenarioNodes) & _scenarioNodeId
          let updateScenarioFile = mkUpdateScenarioFile nodeId envId
          try clientEnv (updateScenarioFileHandler token scenarioCollectionId updateScenarioFile)
          fakeScenarioFile <- selectFakeScenarioFile nodeId connection
          _fakeScenarioFileEnvironmentId fakeScenarioFile `shouldBe` Just envId


-- ** create root scenario file


    describe "create a root scenario file" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, token, accountId } -> do
          (_, newRootScenarioFile) <- mkNewRootScenarioFile nilId accountId connection
          try clientEnv (createRootScenarioFileHandler token nilId newRootScenarioFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the scenario file" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          scenarioCollectionId <- insertFakeScenarioCollection accountId connection
          (environmentId, newRootScenarioFile) <- mkNewRootScenarioFile nilId accountId connection
          _ <- try clientEnv (createRootScenarioFileHandler token scenarioCollectionId newRootScenarioFile)
          fakeScenarioFile <- selectFakeScenarioFile nilId connection
          fakeScenarioFile `shouldBe` FakeScenarioFile { _fakeScenarioFileParentId = Nothing
                                                       , _fakeScenarioFileName = "new scenario"
                                                       , _fakeScenarioFileSceneActorId = Nothing
                                                       , _fakeScenarioFileEnvironmentId = Just environmentId
                                                       }



  where
    mkNewScenarioFile :: Id Scenario -> Id Scenario -> Id Account -> PG.Connection -> IO (Id EnvId, NewScenarioFile)
    mkNewScenarioFile id parentId accountId connection = do
      let newEnvironment = NewFakeEnvironment { _newFakeEnvironmentAccountId = accountId
                                              , _newFakeEnvironmentName      = "env"
                                              }
      envId <- insertNewFakeEnvironment newEnvironment connection
      return ( envId
             , NewScenarioFile { _newScenarioFileId           = id
                               , _newScenarioFileParentNodeId = parentId
                               , _newScenarioFileName = "new scenario"
                               , _newScenarioFileEnvironmentId = Just envId
                               }
             )

    mkUpdateScenarioFile :: Id Scenario -> Id EnvId -> UpdateScenarioFile
    mkUpdateScenarioFile scenarioFileId envId =
      UpdateScenarioFile { _updateScenarioFileId           = scenarioFileId
                         , _updateScenarioFileEnvironmentId = Just envId
                         }

    mkNewRootScenarioFile :: Id Scenario -> Id Account -> PG.Connection -> IO (Id EnvId, NewRootScenarioFile)
    mkNewRootScenarioFile id accountId connection = do
      let newEnvironment = NewFakeEnvironment { _newFakeEnvironmentAccountId = accountId
                                              , _newFakeEnvironmentName      = "env"
                                              }
      envId <- insertNewFakeEnvironment newEnvironment connection
      return ( envId
             , NewRootScenarioFile { _newRootScenarioFileId = id
                                   , _newRootScenarioFileName = "new scenario"
                                   , _newRootScenarioFileEnvironmentId = Just envId
                                   }
             )
