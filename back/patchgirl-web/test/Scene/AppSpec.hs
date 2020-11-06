{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Scene.AppSpec where

import           Data.Coerce                            (coerce)
import           Data.Function                          ((&))
import qualified Data.Map.Strict                        as Map
import qualified Data.Maybe                             as Maybe
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
import           PatchGirl.Web.RequestCollection.Model
import           PatchGirl.Web.RequestNode.Model
import           PatchGirl.Web.ScenarioCollection.Model
import           PatchGirl.Web.ScenarioNode.Model
import           PatchGirl.Web.Server


-- * client


createSceneHandler :: Auth.Token -> Id Scenario -> NewScene -> ClientM ()
deleteSceneHandler :: Auth.Token -> Id Scenario -> Id Scene -> ClientM ()
updateSceneHandler :: Auth.Token -> Id Scenario -> Id Scene -> UpdateScene -> ClientM ()
createSceneHandler
  :<|> deleteSceneHandler
  :<|> updateSceneHandler =
  client (Proxy :: Proxy (SceneActorApi '[Auth.JWT]))


-- * spec


spec :: Spec
spec =
  withClient (defaultEnv2 >>= mkApp) $ do


-- ** create scene


    describe "create a scene" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } -> do
          let newScene = mkNewScene nilId Nothing nilId mkEmptyVariables "" ""
          try clientEnv (createSceneHandler token nilId newScene) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          _ <- insertSampleScenarioCollection accountId connection
          let newScene = mkNewScene nilId Nothing nilId mkEmptyVariables "" ""
          try clientEnv (createSceneHandler token nilId newScene) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node exists but isn't a scenario file" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection _ scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let folderId = Maybe.fromJust (getFirstScenarioFolder scenarioNodes) & _scenarioNodeId
          let newScene = mkNewScene nilId Nothing nilId mkEmptyVariables "" ""
          try clientEnv (createSceneHandler token folderId newScene) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 if the related request file doesnt belong to the account" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (accountId2, _) <- withAccountAndToken defaultNewFakeAccount2 connection
          (_, ScenarioCollection _ scenarioNodes) <- insertSampleScenarioCollection accountId connection
          RequestCollection _ requestNodes <- insertSampleRequestCollection accountId2 connection
          let requestFileId = coerce $ Maybe.fromJust (getFirstFile requestNodes) & _requestNodeId
          let scenarioFile = Maybe.fromJust (getFirstScenarioFile scenarioNodes)
          let scenarioFileId = scenarioFile & _scenarioNodeId
          let newScene = mkNewScene nilId Nothing requestFileId mkEmptyVariables "" ""
          try clientEnv (createSceneHandler token scenarioFileId newScene) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "creates a root scene" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (RequestCollection _ requestNodes, ScenarioCollection _ scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let requestFileId = Maybe.fromJust (getFirstFile requestNodes) & _requestNodeId
          let scenarioFile = Maybe.fromJust (getFirstScenarioFile scenarioNodes)
          let scenarioFileId = scenarioFile & _scenarioNodeId
          let scenarioFirstScene = head $ scenarioFile & _scenarioNodeScenes
          let newScene = mkNewScene nilId Nothing (coerce requestFileId) (SceneVariables (Map.fromList [ ("foo", SceneVariableValue "fee" True)
                                                                                                 , ("bar", SceneVariableValue "baz" False)
                                                                                                 ])) "" ""
          try clientEnv (createSceneHandler token scenarioFileId newScene)
          newCreatedScene <- selectFakeScene nilId connection
          newCreatedScene `shouldBe` Just (FakeScene { _fakeSceneParentId = Nothing
                                                     , _fakeSceneId = coerce requestFileId
                                                     , _fakeActorType = HttpActor
                                                     , _fakeSceneVariables = SceneVariables (Map.fromList[ ("foo", SceneVariableValue "fee" True)
                                                                                                    , ("bar", SceneVariableValue "baz" False)
                                                                                                    ])
                                                     , _fakeScenePrescript = ""
                                                     , _fakeScenePostscript = ""
                                                     })
          newSon <- selectFakeSceneWithParentId nilId connection
          newSon `shouldBe` Just (FakeScene { _fakeSceneParentId = Just nilId
                                            , _fakeSceneId = Id $
                                              case scenarioFirstScene & _sceneActorId of
                                                PostgresSceneId id -> coerce id
                                                RequestSceneId id  -> coerce id
                                            , _fakeActorType = HttpActor
                                            , _fakeSceneVariables = mkEmptyVariables
                                            , _fakeScenePrescript = ""
                                            , _fakeScenePostscript = ""
                                            })

      it "creates an http scene" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (RequestCollection _ requestNodes, ScenarioCollection _ scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let requestFileId = Maybe.fromJust (getFirstFile requestNodes) & _requestNodeId
          let scenarioFile = Maybe.fromJust (getFirstScenarioFile scenarioNodes)
          let scenarioFileId = scenarioFile & _scenarioNodeId
          let scenarioFirstScene = head $ scenarioFile & _scenarioNodeScenes
          let newScene = mkNewScene nilId (Just $ Id $ coerce $ scenarioFirstScene & _sceneId) (Id $ coerce requestFileId) (SceneVariables (Map.fromList [ ("foo", SceneVariableValue "fee" True)
                                                                                                                           , ("bar", SceneVariableValue "baz" False)
                                                                                                                           ])) "" ""
          try clientEnv (createSceneHandler token scenarioFileId newScene)
          newCreatedScene <- selectFakeScene nilId connection
          newCreatedScene `shouldBe` Just (FakeScene { _fakeSceneParentId = Just $ scenarioFirstScene & _sceneId
                                                     , _fakeSceneId = coerce requestFileId
                                                     , _fakeActorType = HttpActor
                                                     , _fakeSceneVariables = SceneVariables (Map.fromList [ ("foo", SceneVariableValue "fee" True)
                                                                                                     , ("bar", SceneVariableValue "baz" False)
                                                                                                     ])
                                                     , _fakeScenePrescript = ""
                                                     , _fakeScenePostscript = ""
                                                     })

      it "creates a pg scene" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (RequestCollection _ requestNodes, ScenarioCollection _ scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let requestFileId = Maybe.fromJust (getFirstFile requestNodes) & _requestNodeId
          let scenarioFile = Maybe.fromJust (getFirstScenarioFile scenarioNodes)
          let scenarioFileId = scenarioFile & _scenarioNodeId
          let scenarioFirstScene = head $ scenarioFile & _scenarioNodeScenes
          let newScene = mkNewScene nilId (Just $ scenarioFirstScene & _sceneId) (coerce requestFileId) (SceneVariables (Map.fromList [ ("foo", SceneVariableValue "fee" True)
                                                                                                                                , ("bar", SceneVariableValue "baz" False)
                                                                                                                                ])) "" ""
          try clientEnv (createSceneHandler token scenarioFileId newScene)
          newCreatedScene <- selectFakeScene nilId connection
          newCreatedScene `shouldBe` Just (FakeScene { _fakeSceneParentId = Just $ scenarioFirstScene & _sceneId
                                                     , _fakeSceneId = coerce requestFileId
                                                     , _fakeActorType = HttpActor
                                                     , _fakeSceneVariables = SceneVariables (Map.fromList [ ("foo", SceneVariableValue "fee" True)
                                                                                                          , ("bar", SceneVariableValue "baz" False)
                                                                                                          ])
                                                     , _fakeScenePrescript = ""
                                                     , _fakeScenePostscript = ""
                                                     })


-- ** delete scene


    describe "delete a scene" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } ->
          try clientEnv (deleteSceneHandler token nilId nilId) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          _ <- insertSampleScenarioCollection accountId connection
          try clientEnv (deleteSceneHandler token nilId nilId) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node exists but is a scenario folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection _ scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let folderId = Maybe.fromJust (getFirstScenarioFolder scenarioNodes) & _scenarioNodeId
          try clientEnv (deleteSceneHandler token folderId nilId) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "delete a scene" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (RequestCollection _ _, ScenarioCollection _ scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let scenarioFile = Maybe.fromJust (getFirstScenarioFile scenarioNodes)
          let scenarioFileId = scenarioFile & _scenarioNodeId
          let sceneId' = head (scenarioFile & _scenarioNodeScenes) & _sceneId
          try clientEnv (deleteSceneHandler token scenarioFileId sceneId')
          selectFakeScene sceneId' connection >>= (`shouldSatisfy` Maybe.isNothing)


-- ** update scene


    describe "update a scene" $ do
      it "returns 404 when scenario collection doesnt exist" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { token } -> do
          let updateScene = mkUpdateScene mkEmptyVariables "" ""
          try clientEnv (updateSceneHandler token nilId nilId updateScene) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns 404 when scenario node exists but is a scenario folder" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (_, ScenarioCollection _ scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let updateScene = mkUpdateScene mkEmptyVariables "" ""
          let folderId = Maybe.fromJust (getFirstScenarioFolder scenarioNodes) & _scenarioNodeId
          try clientEnv (updateSceneHandler token folderId nilId updateScene) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "update a scene" $ \clientEnv ->
        cleanDBAndCreateAccount $ \Test { connection, accountId, token } -> do
          (RequestCollection _ _, ScenarioCollection _ scenarioNodes) <- insertSampleScenarioCollection accountId connection
          let scenarioFile = Maybe.fromJust (getFirstScenarioFile scenarioNodes)
          let scenarioFileId = scenarioFile & _scenarioNodeId
          let sceneId' = head (scenarioFile & _scenarioNodeScenes) & _sceneId
          let updateScene = mkUpdateScene (SceneVariables (Map.fromList [ ("foo", SceneVariableValue "fee" True)
                                                                   , ("bar", SceneVariableValue "baz" False)
                                                                   ])) "prescript!" "postscript!"
          try clientEnv (updateSceneHandler token scenarioFileId sceneId' updateScene)
          FakeScene{..} <- Maybe.fromJust <$> selectFakeScene sceneId' connection
          _fakeScenePrescript `shouldBe` "prescript!"



  where
    mkNewScene :: Id Scene -> Maybe (Id Scene) -> Id Request -> SceneVariables -> String -> String -> NewScene
    mkNewScene id parentId actorId variables prescript postscript =
      NewScene { _newSceneId = id
               , _newSceneSceneActorParentId = parentId
               , _newSceneActorType = HttpActor
               , _newSceneActorId = RequestSceneId actorId
               , _newSceneVariables = variables
               , _newScenePrescript = prescript
               , _newScenePostscript = postscript
               }

    mkUpdateScene :: SceneVariables -> String -> String -> UpdateScene
    mkUpdateScene variables prescript postscript =
      UpdateScene { _updateSceneVariables = variables
                  , _updateScenePrescript = prescript
                  , _updateScenePostscript = postscript
                  }

    mkEmptyVariables :: SceneVariables
    mkEmptyVariables = SceneVariables Map.empty
