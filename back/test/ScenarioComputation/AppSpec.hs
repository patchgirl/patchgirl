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
import qualified Network.HTTP.Client       as HTTP
import qualified Network.HTTP.Types        as HTTP
import           Servant
import qualified Servant.Auth.Client       as Auth
import           Servant.Auth.Server       (JWT)
import           Servant.Client
import           Test.Hspec

import           FakeHttpRequest
import           Helper.App
import qualified Http
import           Interpolator
import           RequestComputation.Model
import           ScenarioComputation.Model
import           TangoScript


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
          ( ScenarioInput { _scenarioInputId = UUID.nil
                          , _scenarioInputScenes = []
                          , _scenarioInputGlobalEnv = Map.fromList []
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
          [ (buildRequest "GET http://foo.com", Right buildResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildSceneInput Http.Get [Sentence "foo.com"] ]
            , _scenarioInputGlobalEnv = Map.fromList []
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
          [ (buildRequest "GET http://foo.com", Right buildResponse)
          , (buildRequest "POST http://foo.com", Left $ HTTP.InvalidUrlException "" "")
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildSceneInput Http.Get [Sentence "foo.com"]
                                     , buildSceneInput Http.Post [Sentence "foo.com"]
                                     ]
            , _scenarioInputGlobalEnv = Map.fromList []
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
          , (buildRequest "POST http://foo.com", Right buildResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildSceneInput Http.Get [Sentence "foo.com"]
                                     , buildSceneInput Http.Post [Sentence "foo.com"]
                                     ]
            , _scenarioInputGlobalEnv = Map.fromList []
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


-- ** prescript fails: cannot `assertEqual`


  describe "prescript fails: cannot `assertEqual`" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildSceneInputWithScript Http.Get [Sentence "foo.com"] [ AssertEqual (LString "a") (LString "b") ] [] ]
            , _scenarioInputGlobalEnv = Map.fromList []
            }
          , ScenarioOutput
            [ buildSceneOutput $ PrescriptFailed $ AssertEqualFailed (LString "a") (LString "b")
            ]
          )

    withClient (withHttpMock2 mock) $
      it "fails" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runScenarioComputation token input) `shouldReturn` output


-- ** prescript fails: trying to access unknown local variable


  describe "prescript fails: trying to access unknown local variable" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildSceneInputWithScript Http.Get [Sentence "foo.com"] [ Let "myVar" (Fetch "unknownVariable") ] [] ]
            , _scenarioInputGlobalEnv = Map.fromList []
            }
          , ScenarioOutput
            [ buildSceneOutput $ PrescriptFailed $ UnknownVariable $ Fetch "unknownVariable"
            ]
          )

    withClient (withHttpMock2 mock) $
      it "fails" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runScenarioComputation token input) `shouldReturn` output


-- ** prescript succeed: assertEqual


  describe "prescript succeed: assertEqual" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildSceneInputWithScript Http.Get [Sentence "foo.com"] [ AssertEqual (LString "a") (LString "a") ] [] ]
            , _scenarioInputGlobalEnv = Map.fromList []
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
      it "fails" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runScenarioComputation token input) `shouldReturn` output


-- ** prescript succeed: set global variable for next scene prescript


  describe "prescript succeed: set global variable for next scene prescript" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildSceneInputWithScript Http.Get [Sentence "foo.com"] [ Set "a" (LInt 1) ] []
                                     , buildSceneInputWithScript Http.Get [Sentence "foo.com"] [ AssertEqual (Fetch "a") (LInt 1) ] []
                                     ]
            , _scenarioInputGlobalEnv = Map.fromList []
            }
          , ScenarioOutput
            [ buildSceneOutput $ SceneSucceeded $ RequestComputationOutput
                { _requestComputationOutputStatusCode = 200
                , _requestComputationOutputHeaders    = []
                , _requestComputationOutputBody       = ""
                }
            , buildSceneOutput $ SceneSucceeded $ RequestComputationOutput
                { _requestComputationOutputStatusCode = 200
                , _requestComputationOutputHeaders    = []
                , _requestComputationOutputBody       = ""
                }
            ]
          )

    withClient (withHttpMock2 mock) $
      it "succeed" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runScenarioComputation token input) `shouldReturn` output


-- ** postscript succeed: assert equal http body response


  describe "postscript succeed: assert equal http body response" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right $ buildResponse { httpResponseBody = "foo" } )
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildSceneInputWithScript Http.Get [Sentence "foo.com"] [] [ AssertEqual HttpResponseBodyAsString (LString "foo") ]
                                     ]
            , _scenarioInputGlobalEnv = Map.fromList []
            }
          , ScenarioOutput
            [ buildSceneOutput $ SceneSucceeded $ RequestComputationOutput
                { _requestComputationOutputStatusCode = 200
                , _requestComputationOutputHeaders    = []
                , _requestComputationOutputBody       = "foo"
                }
            ]
          )

    withClient (withHttpMock2 mock) $
      it "succeed" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runScenarioComputation token input) `shouldReturn` output


-- ** postscript fails: assert equal http body response


  describe "postscript: fail assert equal http body response" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right $ buildResponse { httpResponseBody = "foo" } )
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildSceneInputWithScript Http.Get [Sentence "foo.com"] [] [ AssertEqual HttpResponseBodyAsString (LString "bar") ]
                                     ]
            , _scenarioInputGlobalEnv = Map.fromList []
            }
          , ScenarioOutput
            [ buildSceneOutput $ PostscriptFailed $ AssertEqualFailed (LString "foo") (LString "bar")
            ]
          )

    withClient (withHttpMock2 mock) $
      it "fails" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runScenarioComputation token input) `shouldReturn` output


-- ** postscript succeed: set global variable for next scene postscript


  describe "postscript succeed: set global variable for next scene postscript" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildSceneInputWithScript Http.Get [Sentence "foo.com"] [] [ Set "a" (LInt 1) ]
                                     , buildSceneInputWithScript Http.Get [Sentence "foo.com"] [] [ AssertEqual (Fetch "a") (LInt 1) ]
                                     ]
            , _scenarioInputGlobalEnv = Map.fromList []
            }
          , ScenarioOutput
            [ buildSceneOutput $ SceneSucceeded $ RequestComputationOutput
                { _requestComputationOutputStatusCode = 200
                , _requestComputationOutputHeaders    = []
                , _requestComputationOutputBody       = ""
                }
            , buildSceneOutput $ SceneSucceeded $ RequestComputationOutput
                { _requestComputationOutputStatusCode = 200
                , _requestComputationOutputHeaders    = []
                , _requestComputationOutputBody       = ""
                }
            ]
          )

    withClient (withHttpMock2 mock) $
      it "succeed" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runScenarioComputation token input) `shouldReturn` output


-- ** pre/postscript succeed: set global variable from prescript to next scene postscript


  describe "pre/postscript succeed: set global variable from prescript to next scene postscript" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildSceneInputWithScript Http.Get [Sentence "foo.com"] [ Set "a" (LInt 1) ] []
                                     , buildSceneInputWithScript Http.Get [Sentence "foo.com"] [] [ AssertEqual (Fetch "a") (LInt 1) ]
                                     ]
            , _scenarioInputGlobalEnv = Map.fromList []
            }
          , ScenarioOutput
            [ buildSceneOutput $ SceneSucceeded $ RequestComputationOutput
                { _requestComputationOutputStatusCode = 200
                , _requestComputationOutputHeaders    = []
                , _requestComputationOutputBody       = ""
                }
            , buildSceneOutput $ SceneSucceeded $ RequestComputationOutput
                { _requestComputationOutputStatusCode = 200
                , _requestComputationOutputHeaders    = []
                , _requestComputationOutputBody       = ""
                }
            ]
          )

    withClient (withHttpMock2 mock) $
      it "succeed" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (runScenarioComputation token input) `shouldReturn` output


-- * util


-- ** build input scene


buildSceneInputWithScript :: Http.Method -> [TemplatedString] -> TangoAst -> TangoAst -> SceneInput
buildSceneInputWithScript method url prescript postscript =
  SceneInput { _sceneInputId = UUID.nil
             , _sceneInputRequestFileNodeId = UUID.nil
             , _sceneInputPrescript = prescript
             , _sceneInputPostscript = postscript
             , _sceneInputTemplatedRequestComputationInput =
               Just $ defaultRequestComputationInput { _templatedRequestComputationInputMethod = method
                                                     , _templatedRequestComputationInputUrl = url
                                                     }
             }

buildSceneInput :: Http.Method -> [TemplatedString] -> SceneInput
buildSceneInput method url =
  buildSceneInputWithScript method url [] []


-- ** build output scene


buildSceneOutput :: SceneComputation -> SceneOutput
buildSceneOutput sceneComputation =
  SceneOutput { _outputSceneId = UUID.nil
              , _outputSceneRequestFileNodeId = UUID.nil
              , _outputSceneComputation = sceneComputation
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
