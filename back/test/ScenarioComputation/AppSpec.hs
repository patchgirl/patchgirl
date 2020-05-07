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
import           TangoScript
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
          [ (buildRequest "GET http://foo.com", Right buildResponse)
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
          [ (buildRequest "GET http://foo.com", Right buildResponse)
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
          , (buildRequest "POST http://foo.com", Right buildResponse)
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


-- ** prescript fails: cannot `assertEqual`


  describe "prescript fails: cannot `assertEqual`" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _inputScenarioId = UUID.nil
            , _inputScenarioScenes = [ buildSceneInputWithScript Http.Get "foo.com" [ AssertEqual (LString "a") (LString "b") ] [] ]
            , _inputScenarioGlobalEnv = Map.fromList []
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
            { _inputScenarioId = UUID.nil
            , _inputScenarioScenes = [ buildSceneInputWithScript Http.Get "foo.com" [ Let "myVar" (Get "unknownVariable") ] [] ]
            , _inputScenarioGlobalEnv = Map.fromList []
            }
          , ScenarioOutput
            [ buildSceneOutput $ PrescriptFailed $ UnknownVariable $ Get "unknownVariable"
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
            { _inputScenarioId = UUID.nil
            , _inputScenarioScenes = [ buildSceneInputWithScript Http.Get "foo.com" [ AssertEqual (LString "a") (LString "a") ] [] ]
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
            { _inputScenarioId = UUID.nil
            , _inputScenarioScenes = [ buildSceneInputWithScript Http.Get "foo.com" [ Set "a" (LInt 1) ] []
                                     , buildSceneInputWithScript Http.Get "foo.com" [ AssertEqual (Get "a") (LInt 1) ] []
                                     ]
            , _inputScenarioGlobalEnv = Map.fromList []
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
            { _inputScenarioId = UUID.nil
            , _inputScenarioScenes = [ buildSceneInputWithScript Http.Get "foo.com" [] [ AssertEqual HttpResponseBodyAsString (LString "foo") ]
                                     ]
            , _inputScenarioGlobalEnv = Map.fromList []
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
            { _inputScenarioId = UUID.nil
            , _inputScenarioScenes = [ buildSceneInputWithScript Http.Get "foo.com" [] [ AssertEqual HttpResponseBodyAsString (LString "bar") ]
                                     ]
            , _inputScenarioGlobalEnv = Map.fromList []
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
            { _inputScenarioId = UUID.nil
            , _inputScenarioScenes = [ buildSceneInputWithScript Http.Get "foo.com" [] [ Set "a" (LInt 1) ]
                                     , buildSceneInputWithScript Http.Get "foo.com" [] [ AssertEqual (Get "a") (LInt 1) ]
                                     ]
            , _inputScenarioGlobalEnv = Map.fromList []
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
            { _inputScenarioId = UUID.nil
            , _inputScenarioScenes = [ buildSceneInputWithScript Http.Get "foo.com" [ Set "a" (LInt 1) ] []
                                     , buildSceneInputWithScript Http.Get "foo.com" [] [ AssertEqual (Get "a") (LInt 1) ]
                                     ]
            , _inputScenarioGlobalEnv = Map.fromList []
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


buildSceneInputWithScript :: Http.Method -> String -> TangoAst -> TangoAst -> SceneInput
buildSceneInputWithScript method url prescript postscript =
  SceneInput { _inputSceneId = UUID.nil
             , _inputSceneRequestFileNodeId = UUID.nil
             , _inputScenePrescript = prescript
             , _inputScenePostscript = postscript
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

buildSceneInput :: Http.Method -> String -> SceneInput
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
