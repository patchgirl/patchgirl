{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators             #-}

module ScenarioComputation.AppSpec where

import           App
import qualified Data.ByteString.UTF8      as BSU
import qualified Data.Map.Strict           as Map
import qualified Data.UUID                 as UUID
import           FakeHttpRequest
import           Helper.App
import qualified Http
import qualified Network.HTTP.Client       as HTTP
import qualified Network.HTTP.Types        as HTTP

import           RequestComputation.Model
import           ScenarioComputation.Model
import           Servant
import qualified Servant.Auth.Client       as Auth
import           Servant.Auth.Server       (JWT)
import           Servant.Client
import           Test.Hspec


-- * client


runScenarioComputation :: Auth.Token -> ScenarioInput -> ClientM ScenarioOutput
runScenarioComputation =
  client (Proxy :: Proxy (ScenarioComputationApi '[JWT]))


-- * spec


spec :: Spec
spec = do


-- ** empty scenario


  describe "empty scenario" $ do

    let (input, output) =
          ( ScenarioInput { _inputScenarioId = UUID.nil
                          , _inputScenarioScenes = []
                          , _inputScenarioGlobalEnv = Map.fromList []
                          }
          , ScenarioOutput []
          )

    withClient (mkApp defaultEnv) $
      it "runs empty scenario" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runScenarioComputation token input) `shouldReturn` output


-- ** scenario with one valid scene


  describe "scenario with one valid scene" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right $ buildResponse 200)
          ]

    let (input, output) =
          ( ScenarioInput
            { _inputScenarioId = UUID.nil
            , _inputScenarioScenes = [ buildSceneInput Http.Get "foo.com" ]
            , _inputScenarioGlobalEnv = Map.fromList []
            }
          , ScenarioOutput
            [ buildSceneOutput $ SceneSucceeded $ RequestComputationOutput
                { _requestComputationOutputStatusCode = 200
                , _requestComputationOutputHeaders    = []
                , _requestComputationOutputBody       = ""
                }
            ]
          )

    withClient (withHttpMock2 mock) $
      it "runs a single scene scenario" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runScenarioComputation token input) `shouldReturn` output


-- ** scenario with last scene invalid (invalid url)


  describe "scenario with last scene invalid" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right $ buildResponse 200)
          , (buildRequest "POST http://foo.com", Left $ HTTP.InvalidUrlException "" "")
          ]

    let (input, output) =
          ( ScenarioInput
            { _inputScenarioId = UUID.nil
            , _inputScenarioScenes = [ buildSceneInput Http.Get "foo.com"
                                     , buildSceneInput Http.Post "foo.com"
                                     ]
            , _inputScenarioGlobalEnv = Map.fromList []
            }
          , ScenarioOutput
            [ buildSceneOutput $ SceneSucceeded $ RequestComputationOutput
                { _requestComputationOutputStatusCode = 200
                , _requestComputationOutputHeaders    = []
                , _requestComputationOutputBody       = ""
                }
            , buildSceneOutput $ RequestFailed $ InvalidUrlException "" ""
            ]
          )

    withClient (withHttpMock2 mock) $
      it "run all scenes" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runScenarioComputation token input) `shouldReturn` output


-- ** scenario first scene invalid (invalid url)


  describe "scenario with first scene invalid" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Left $ HTTP.InvalidUrlException "" "")
          , (buildRequest "POST http://foo.com", Right $ buildResponse 200 )
          ]

    let (input, output) =
          ( ScenarioInput
            { _inputScenarioId = UUID.nil
            , _inputScenarioScenes = [ buildSceneInput Http.Get "foo.com"
                                     , buildSceneInput Http.Post "foo.com"
                                     ]
            , _inputScenarioGlobalEnv = Map.fromList []
            }
          , ScenarioOutput
            [ buildSceneOutput $ RequestFailed $ InvalidUrlException "" ""
            , buildSceneOutput SceneNotRun
            ]
          )

    withClient (withHttpMock2 mock) $
      it "doesnt run scenes after a failing scene" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runScenarioComputation token input) `shouldReturn` output


-- * util


-- ** build input scene


buildSceneInput :: Http.Method -> String -> SceneInput
buildSceneInput method url =
  SceneInput { _inputSceneId = UUID.nil
             , _inputSceneRequestFileNodeId = UUID.nil
             , _inputScenePreScript = []
             , _inputSceneRequestComputationInput = Just requestComputationInput
             }
  where
    requestComputationInput =
      RequestComputationInput { _requestComputationInputMethod = method
                              , _requestComputationInputHeaders = []
                              , _requestComputationInputScheme = Http.Http
                              , _requestComputationInputUrl = url
                              , _requestComputationInputBody = ""
                              }


-- ** build output scene


buildSceneOutput :: SceneComputation -> SceneOutput
buildSceneOutput sceneComputation =
  SceneOutput { _outputSceneId = UUID.nil
              , _outputSceneRequestFileNodeId = UUID.nil
              , _outputSceneComputation = sceneComputation
              }


-- ** build response


buildResponse :: Int -> HttpResponse BSU.ByteString
buildResponse statusCode =
  HttpResponse { httpResponseStatus = HTTP.Status { statusCode = statusCode
                                                  , statusMessage = BSU.fromString "ok"
                                                  }
               , httpResponseHeaders = []
               , httpResponseBody = BSU.fromString ""
               }
