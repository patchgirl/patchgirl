{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module ScenarioFolder.AppSpec where

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


createScenarioFolder :: Auth.Token -> Id ScenarioCol -> NewScenarioFolder -> ClientM ()
createRootScenarioFolder :: Auth.Token -> Id ScenarioCol -> NewRootScenarioFolder -> ClientM ()
createScenarioFolder :<|> createRootScenarioFolder =
  client (Proxy :: Proxy (ScenarioFolderApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultEnv) $ do


-- ** create scenario folder


    describe "create a scenario folder" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } -> do
          let newScenarioFolder = mkNewScenarioFolder nilId nilId
          try clientEnv (createScenarioFolder token nilId newScenarioFolder) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node parent doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId _) <- insertSampleScenarioCollection accountId connection
          let newScenarioFolder = mkNewScenarioFolder nilId nilId
          try clientEnv (createScenarioFolder token scenarioCollectionId newScenarioFolder) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node parent exist but isn't a scenario folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let fileId = Maybe.fromJust (getFirstScenarioFile scenarioNodes) & _scenarioNodeId
          let newScenarioFolder = mkNewScenarioFolder nilId fileId
          try clientEnv (createScenarioFolder token scenarioCollectionId newScenarioFolder) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the scenario folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let folderId = Maybe.fromJust (getFirstScenarioFolder scenarioNodes) & _scenarioNodeId
          let newScenarioFolder = mkNewScenarioFolder nilId folderId
          _ <- try clientEnv (createScenarioFolder token scenarioCollectionId newScenarioFolder)
          fakeScenarioFolder <- selectFakeScenarioFolder nilId connection
          fakeScenarioFolder `shouldBe`  FakeScenarioFolder { _fakeScenarioFolderParentId   = Just folderId
                                                            , _fakeScenarioFolderName       = "whatever"
                                                            }


-- ** create root scenario folder


    describe "create a root scenario folder" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } -> do
          let newRootScenarioFolder = mkNewRootScenarioFolder nilId
          try clientEnv (createRootScenarioFolder token nilId newRootScenarioFolder) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the scenario folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          scenarioCollectionId <- insertFakeScenarioCollection accountId connection
          let newRootScenarioFolder = mkNewRootScenarioFolder nilId
          _ <- try clientEnv (createRootScenarioFolder token scenarioCollectionId newRootScenarioFolder)
          fakeScenarioFolder <- selectFakeScenarioFolder nilId connection
          fakeScenarioFolder `shouldBe`  FakeScenarioFolder { _fakeScenarioFolderParentId = Nothing
                                                            , _fakeScenarioFolderName       = "test"
                                                            }

  where
    mkNewScenarioFolder :: Id Scenario -> Id Scenario -> NewScenarioFolder
    mkNewScenarioFolder id parentId =
      NewScenarioFolder { _newScenarioFolderId           = id
                        , _newScenarioFolderParentNodeId = parentId
                        , _newScenarioFolderName         = "whatever"
                        }

    mkNewRootScenarioFolder :: Id Scenario -> NewRootScenarioFolder
    mkNewRootScenarioFolder id =
      NewRootScenarioFolder { _newRootScenarioFolderId = id
                            , _newRootScenarioFolderName = "test"
                            }
