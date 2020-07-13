{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module ScenarioComputation.AppSpec where

import qualified Data.ByteString.UTF8      as BSU
import qualified Data.Map.Strict           as Map
import qualified Data.UUID                 as UUID
import qualified Network.HTTP.Client       as HTTP
import qualified Network.HTTP.Types        as HTTP
import           Servant
import qualified Servant.Client            as Servant
import           Test.Hspec

import           Api
import           FakeHttpRequest
import           Helper.App
import           Http
import           Interpolator
import           RequestComputation.Model
import           ScenarioComputation.Model
import           Server
import           TangoScript


-- * client


runScenarioComputation :: ScenarioInput -> Servant.ClientM ScenarioOutput
runScenarioComputation =
  Servant.client (Proxy :: Proxy ScenarioComputationApi)


-- * spec


spec :: Spec
spec = do


-- ** empty scenario


  describe "empty scenario" $ do

    let (input, output) =
          ( ScenarioInput { _scenarioInputId = UUID.nil
                          , _scenarioInputScenes = []
                          , _scenarioInputEnvVars = Map.empty
                          }
          , ScenarioOutput []
          )

    withClient (mkApp defaultEnv) $
      it "runs empty scenario" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- ** scenario with one valid scene


  describe "scenario with one valid scene" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildScene Get [Sentence "http://foo.com"] ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ buildSceneOutput $ HttpSceneOk requestComputation
            ]
          )

    withClient (withHttpMock2 mock) $
      it "runs a single scene scenario" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- ** scenario with last scene invalid (invalid url)


  describe "scenario with last scene invalid" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildResponse)
          , (buildRequest "POST http://foo.com", Left $ HTTP.InvalidUrlException "" "")
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildScene Get [Sentence "http://foo.com"]
                                     , buildScene Post [Sentence "http://foo.com"]
                                     ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ buildSceneOutput $ HttpSceneOk requestComputation
            , buildSceneOutput $ HttpSceneFailed $ InvalidUrlException "" ""
            ]
          )

    withClient (withHttpMock2 mock) $
      it "run all scenes" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- ** scenario first scene invalid (invalid url)


  describe "scenario with first scene invalid" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Left $ HTTP.InvalidUrlException "" "")
          , (buildRequest "POST http://foo.com", Right buildResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildScene Get [Sentence "http://foo.com"]
                                     , buildScene Post [Sentence "http://foo.com"]
                                     ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ buildSceneOutput $ HttpSceneFailed $ InvalidUrlException "" ""
            , buildSceneOutput SceneNotRun
            ]
          )

    withClient (withHttpMock2 mock) $
      it "doesnt run scenes after a failing scene" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- ** prescript fails: cannot `assertEqual`


  describe "prescript fails: cannot `assertEqual`" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildHttpSceneWithScript Get [Sentence "foo.com"] [ AssertEqual (LString "a") (LString "b") ] [] ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ buildSceneOutput $ PrescriptFailed $ AssertEqualFailed (LString "a") (LString "b")
            ]
          )

    withClient (withHttpMock2 mock) $
      it "fails" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- ** prescript fails: trying to access unknown local variable


  describe "prescript fails: trying to access unknown local variable" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildHttpSceneWithScript Get [Sentence "foo.com"] [ Let "myVar" (Fetch "unknownVariable") ] [] ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ buildSceneOutput $ PrescriptFailed $ UnknownVariable $ Fetch "unknownVariable"
            ]
          )

    withClient (withHttpMock2 mock) $
      it "fails" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- ** prescript succeed: assertEqual


  describe "prescript succeed: assertEqual" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildHttpSceneWithScript Get [Sentence "http://foo.com"] [ AssertEqual (LString "a") (LString "a") ] [] ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ buildSceneOutput $ HttpSceneOk requestComputation
            ]
          )

    withClient (withHttpMock2 mock) $
      it "fails" $ \clientEnv ->
          try clientEnv (runScenarioComputation input) `shouldReturn` output


-- ** prescript succeed: set global variable for next scene prescript


  describe "prescript succeed: set global variable for next scene prescript" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildHttpSceneWithScript Get [Sentence "http://foo.com"] [ Set "a" (LInt 1) ] []
                                     , buildHttpSceneWithScript Get [Sentence "http://foo.com"] [ AssertEqual (Fetch "a") (LInt 1) ] []
                                     ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ buildSceneOutput $ HttpSceneOk requestComputation
            , buildSceneOutput $ HttpSceneOk requestComputation
            ]
          )

    withClient (withHttpMock2 mock) $
      it "succeed" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- ** postscript succeed: assert equal http body response


  describe "postscript succeed: assert equal http body response" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right $ buildResponse { httpResponseBody = "foo" } )
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildHttpSceneWithScript Get [Sentence "http://foo.com"] [] [ AssertEqual HttpResponseBodyAsString (LString "foo") ]
                                     ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ buildSceneOutput $ HttpSceneOk $ requestComputation { _requestComputationBody = "foo" }
            ]
          )

    withClient (withHttpMock2 mock) $
      it "succeed" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- ** postscript fails: assert equal http body response


  describe "postscript: fail assert equal http body response" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right $ buildResponse { httpResponseBody = "foo" } )
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildHttpSceneWithScript Get [Sentence "http://foo.com"] [] [ AssertEqual HttpResponseBodyAsString (LString "bar") ]
                                     ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ buildSceneOutput $ HttpPostscriptFailed (requestComputation { _requestComputationBody = "foo" }) (AssertEqualFailed (LString "foo") (LString "bar"))
            ]
          )

    withClient (withHttpMock2 mock) $
      it "fails" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- ** postscript succeed: set global variable for next scene postscript


  describe "postscript succeed: set global variable for next scene postscript" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildHttpSceneWithScript Get [Sentence "http://foo.com"] [] [ Set "a" (LInt 1) ]
                                     , buildHttpSceneWithScript Get [Sentence "http://foo.com"] [] [ AssertEqual (Fetch "a") (LInt 1) ]
                                     ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ buildSceneOutput $ HttpSceneOk requestComputation
            , buildSceneOutput $ HttpSceneOk requestComputation
            ]
          )

    withClient (withHttpMock2 mock) $
      it "succeed" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- ** pre/postscript succeed: set global variable from prescript to next scene postscript


  describe "pre/postscript succeed: set global variable from prescript to next scene postscript" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildHttpSceneWithScript Get [Sentence "http://foo.com"] [ Set "a" (LInt 1) ] []
                                     , buildHttpSceneWithScript Get [Sentence "http://foo.com"] [] [ AssertEqual (Fetch "a") (LInt 1) ]
                                     ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ buildSceneOutput $ HttpSceneOk requestComputation
            , buildSceneOutput $ HttpSceneOk requestComputation
            ]
          )

    withClient (withHttpMock2 mock) $
      it "succeed" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- * util


-- ** build input scene


buildHttpSceneWithScript :: Method -> StringTemplate -> TangoAst -> TangoAst -> SceneFile
buildHttpSceneWithScript method url prescript postscript =
  HttpSceneFile { _sceneId = UUID.nil
                , _sceneFileId = UUID.nil
                , _scenePrescript = prescript
                , _scenePostscript = postscript
                , _sceneHttpInput = defaultRequestComputationInput { _templatedRequestComputationInputMethod = method
                                                                   , _templatedRequestComputationInputUrl = url
                                                                   }
                }

buildScene :: Method -> StringTemplate -> SceneFile
buildScene method url =
  buildHttpSceneWithScript method url [] []


-- ** build output scene


buildSceneOutput :: SceneComputation -> SceneOutput
buildSceneOutput sceneComputationOutput =
  SceneOutput { _outputSceneId = UUID.nil
              , _outputSceneRequestFileNodeId = UUID.nil
              , _outputSceneComputation = sceneComputationOutput
              }


-- ** build response


buildResponse :: HttpResponse BSU.ByteString
buildResponse =
  HttpResponse { httpResponseStatus = HTTP.Status { statusCode = 200
                                                  , statusMessage = BSU.fromString "ok"
                                                  }
               , httpResponseHeaders = []
               , httpResponseBody = BSU.fromString ""
               }

requestComputation :: RequestComputation
requestComputation =
  RequestComputation { _requestComputationStatusCode = 200
                     , _requestComputationHeaders    = []
                     , _requestComputationBody       = ""
                     }
