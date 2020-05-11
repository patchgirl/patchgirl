{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators             #-}

module ScenarioFile.AppSpec where

import           Control.Lens.Getter        ((^.))
import qualified Data.Maybe                 as Maybe
import           Data.UUID
import qualified Data.UUID                  as UUID
import qualified Database.PostgreSQL.Simple as PG
import qualified Network.HTTP.Types         as HTTP
import           Servant                    hiding (Header)
import qualified Servant.Auth.Client        as Auth
import qualified Servant.Auth.Server        as Auth
import           Servant.Client             (ClientM, client)
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
  withClient (mkApp defaultEnv) $ do


-- ** create scenario file


    describe "create a scenario file" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, token, accountId } -> do
          newScenarioFile <- mkNewScenarioFile UUID.nil UUID.nil accountId connection
          try clientEnv (createScenarioFileHandler token UUID.nil newScenarioFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node parent doesnt exist" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId _) <- insertSampleScenarioCollection accountId connection
          newScenarioFile <- mkNewScenarioFile UUID.nil UUID.nil accountId connection
          try clientEnv (createScenarioFileHandler token scenarioCollectionId newScenarioFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node parent exist but isn't a scenario folder" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let fileId = Maybe.fromJust (getFirstScenarioFile scenarioNodes) ^. scenarioNodeId
          newScenarioFile <- mkNewScenarioFile UUID.nil fileId accountId connection
          try clientEnv (createScenarioFileHandler token scenarioCollectionId newScenarioFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the scenario file" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection scenarioCollectionId scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let folderId = Maybe.fromJust (getFirstScenarioFolder scenarioNodes) ^. scenarioNodeId
          newScenarioFile <- mkNewScenarioFile UUID.nil folderId accountId connection
          _ <- try clientEnv (createScenarioFileHandler token scenarioCollectionId newScenarioFile)
          fakeScenarioFile <- selectFakeScenarioFile UUID.nil connection
          fakeScenarioFile `shouldBe`  FakeScenarioFile { _fakeScenarioFileParentId    = Just folderId
                                                        , _fakeScenarioFileName        = "new scenario"
                                                        , _fakeScenarioFileSceneNodeId = Nothing
                                                        }

-- ** create root scenario file


    describe "create a root scenario file" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, token, accountId } -> do
          newRootScenarioFile <- mkNewRootScenarioFile UUID.nil accountId connection
          try clientEnv (createRootScenarioFileHandler token UUID.nil newRootScenarioFile) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "create the scenario file" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          scenarioCollectionId <- insertFakeScenarioCollection accountId connection
          newRootScenarioFile <- mkNewRootScenarioFile UUID.nil accountId connection
          _ <- try clientEnv (createRootScenarioFileHandler token scenarioCollectionId newRootScenarioFile)
          fakeScenarioFile <- selectFakeScenarioFile UUID.nil connection
          fakeScenarioFile `shouldBe` FakeScenarioFile { _fakeScenarioFileParentId = Nothing
                                                       , _fakeScenarioFileName = "new scenario"
                                                       , _fakeScenarioFileSceneNodeId = Nothing
                                                       }



  where
    mkNewScenarioFile :: UUID -> UUID -> UUID -> PG.Connection -> IO NewScenarioFile
    mkNewScenarioFile id parentId accountId connection = do
      let newEnvironment = NewFakeEnvironment { _newFakeEnvironmentAccountId = accountId
                                              , _newFakeEnvironmentName      = "env"
                                              }
      envId <- insertNewFakeEnvironment newEnvironment connection
      return $ NewScenarioFile { _newScenarioFileId           = id
                               , _newScenarioFileParentNodeId = parentId
                               , _newScenarioFileName = "new scenario"
                               , _newScenarioFileEnvironmentId = Just envId
                               }

    mkNewRootScenarioFile :: UUID -> UUID -> PG.Connection -> IO NewRootScenarioFile
    mkNewRootScenarioFile id accountId connection = do
      let newEnvironment = NewFakeEnvironment { _newFakeEnvironmentAccountId = accountId
                                              , _newFakeEnvironmentName      = "env"
                                              }
      envId <- insertNewFakeEnvironment newEnvironment connection
      return $ NewRootScenarioFile { _newRootScenarioFileId = id
                                   , _newRootScenarioFileName = "new scenario"
                                   , _newRootScenarioFileEnvironmentId = Just envId
                                   }
