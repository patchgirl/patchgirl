{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module ScenarioComputation.AppSpec where

import qualified Data.Map.Strict           as Map
import qualified Data.UUID                 as UUID
import qualified Network.HTTP.Client       as HTTP
import           Servant
import qualified Servant.Client            as Servant
import           Test.Hspec

import           Api
import           FakeHttpRequest
import           Helper.App
import           PatchGirl.Web.Http
import           Interpolator
import           PgSqlComputation.Model
import           RequestComputation.Model
import           ScenarioComputation.Model
import           Server
import           TangoScript.Model


-- * client


runScenarioComputation :: ScenarioInput -> Servant.ClientM ScenarioOutput
runScenarioComputation =
  Servant.client (Proxy :: Proxy ScenarioComputationApi)


-- * spec


spec :: Spec
spec = do

-- ** http
-- *** empty scenario


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


-- *** scenario with one valid scene


  describe "scenario with one valid scene" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildHttpResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildScene Get [Sentence "http://foo.com"] ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ mkSceneOutput $ HttpSceneOk requestComputation
            ]
          )

    withClient (withHttpMock mock) $
      it "runs a single scene scenario" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- *** scenario with last scene invalid (invalid url)


  describe "scenario with last scene invalid" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildHttpResponse)
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
            [ mkSceneOutput $ HttpSceneOk requestComputation
            , mkSceneOutput $ HttpSceneFailed $ InvalidUrlException "" ""
            ]
          )

    withClient (withHttpMock mock) $
      it "run all scenes" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- *** scenario first scene invalid (invalid url)


  describe "scenario with first scene invalid" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Left $ HTTP.InvalidUrlException "" "")
          , (buildRequest "POST http://foo.com", Right buildHttpResponse)
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
            [ mkSceneOutput $ HttpSceneFailed $ InvalidUrlException "" ""
            , mkSceneOutput SceneNotRun
            ]
          )

    withClient (withHttpMock mock) $
      it "doesnt run scenes after a failing scene" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- *** prescript fails: cannot `assertEqual`


  describe "prescript fails: cannot `assertEqual`" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildHttpResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildHttpSceneWithScript Get [Sentence "foo.com"] [ AssertEqual (LString "a") (LString "b") ] [] ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ mkSceneOutput $ PrescriptFailed $ AssertionFailed (LString "a") (LString "b") "LString \"a\" is not equal to LString \"b\""
            ]
          )

    withClient (withHttpMock mock) $
      it "fails" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- *** prescript fails: trying to access unknown local variable


  describe "prescript fails: trying to access unknown local variable" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildHttpResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildHttpSceneWithScript Get [Sentence "foo.com"] [ Let "myVar" (LFetch "unknownVariable") ] [] ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ mkSceneOutput $ PrescriptFailed $ UnknownVariable $ LFetch "unknownVariable"
            ]
          )

    withClient (withHttpMock mock) $
      it "fails" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- *** prescript succeed: assertEqual


  describe "prescript succeed: assertEqual" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildHttpResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildHttpSceneWithScript Get [Sentence "http://foo.com"] [ AssertEqual (LString "a") (LString "a") ] [] ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ mkSceneOutput $ HttpSceneOk requestComputation
            ]
          )

    withClient (withHttpMock mock) $
      it "fails" $ \clientEnv ->
          try clientEnv (runScenarioComputation input) `shouldReturn` output


-- *** prescript succeed: set global variable for next scene prescript


  describe "prescript succeed: set global variable for next scene prescript" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildHttpResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildHttpSceneWithScript Get [Sentence "http://foo.com"] [ Set "a" (LInt 1) ] []
                                     , buildHttpSceneWithScript Get [Sentence "http://foo.com"] [ AssertEqual (LFetch "a") (LInt 1) ] []
                                     ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ mkSceneOutput $ HttpSceneOk requestComputation
            , mkSceneOutput $ HttpSceneOk requestComputation
            ]
          )

    withClient (withHttpMock mock) $
      it "succeed" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- *** postscript succeed: assert equal http body response


  describe "postscript succeed: assert equal http body response" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right $ buildHttpResponse { httpResponseBody = "foo" } )
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildHttpSceneWithScript Get [Sentence "http://foo.com"] [] [ AssertEqual LHttpResponseBodyAsString (LString "foo") ]
                                     ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ mkSceneOutput $ HttpSceneOk $ requestComputation { _requestComputationBody = "foo" }
            ]
          )

    withClient (withHttpMock mock) $
      it "succeed" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- *** postscript fails: assert equal http body response


  describe "postscript: fail assert equal http body response" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right $ buildHttpResponse { httpResponseBody = "foo" } )
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildHttpSceneWithScript Get [Sentence "http://foo.com"] [] [ AssertEqual LHttpResponseBodyAsString (LString "bar") ]
                                     ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ mkSceneOutput $ HttpPostscriptFailed (requestComputation { _requestComputationBody = "foo" }) AssertionFailed (LString "foo") (LString "bar") "LString \"foo\" is not equal to LString \"bar\""
            ]
          )

    withClient (withHttpMock mock) $
      it "fails" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- *** postscript succeed: set global variable for next scene postscript


  describe "postscript succeed: set global variable for next scene postscript" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildHttpResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildHttpSceneWithScript Get [Sentence "http://foo.com"] [] [ Set "a" (LInt 1) ]
                                     , buildHttpSceneWithScript Get [Sentence "http://foo.com"] [] [ AssertEqual (LFetch "a") (LInt 1) ]
                                     ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ mkSceneOutput $ HttpSceneOk requestComputation
            , mkSceneOutput $ HttpSceneOk requestComputation
            ]
          )

    withClient (withHttpMock mock) $
      it "succeed" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- *** pre/postscript succeed: set global variable from prescript to next scene postscript


  describe "pre/postscript succeed: set global variable from prescript to next scene postscript" $ do
    let mock =
          [ (buildRequest "GET http://foo.com", Right buildHttpResponse)
          ]

    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildHttpSceneWithScript Get [Sentence "http://foo.com"] [ Set "a" (LInt 1) ] []
                                     , buildHttpSceneWithScript Get [Sentence "http://foo.com"] [] [ AssertEqual (LFetch "a") (LInt 1) ]
                                     ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ mkSceneOutput $ HttpSceneOk requestComputation
            , mkSceneOutput $ HttpSceneOk requestComputation
            ]
          )

    withClient (withHttpMock mock) $
      it "succeed" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- ** pg


-- *** return simple table


  describe "return simple table" $ do
    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes = [ buildPgSceneWithScript [] [] defaultPgComputationInput ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ mkSceneOutput $ PgSceneOk (PgTuplesOk [ Row [("?column?", PgInt 1)]])
            ]
          )

    withClient (mkApp defaultEnv) $
      it "succeed" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output



-- *** assert eq simple table


  describe "assert simple table" $ do
    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes =
              [ buildPgSceneWithScript
                []
                [ AssertEqual LPgSimpleResponse (LList [ LList [ LInt 1, LInt 2 ] ] ) ]
                (defaultPgComputationInput { _pgComputationInputSql = [ Sentence "select 1, 2;" ]})
              ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ mkSceneOutput $ PgSceneOk (PgTuplesOk [ Row [ ("?column?", PgInt 1), ("?column?", PgInt 2) ] ] )
            ]
          )

    withClient (mkApp defaultEnv) $
      it "succeed" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- *** succeed: access simple table


  describe "succeed: access simple table" $ do
    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes =
              [ buildPgSceneWithScript
                []
                [ AssertEqual
                    (LAccessOp (LAccessOp LPgSimpleResponse (LInt 0)) (LInt 0))
                    (LInt 1)
                ]
                (defaultPgComputationInput { _pgComputationInputSql = [ Sentence "select 1 as \"id\"" ]})
              ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ mkSceneOutput $ PgSceneOk (PgTuplesOk [ Row [ ("id", PgInt 1) ] ] )
            ]
          )

    withClient (mkApp defaultEnv) $
      it "succeed" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- *** fail: access simple table


  describe "fail: access simple table" $ do
    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes =
              [ buildPgSceneWithScript
                []
                [ AssertEqual
                    (LAccessOp (LAccessOp LPgSimpleResponse (LInt 0)) (LInt 2))
                    (LInt 1)
                ]
                (defaultPgComputationInput { _pgComputationInputSql = [ Sentence "select 1 as \"id\"" ]})
              ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ mkSceneOutput $ PgPostscriptFailed (PgTuplesOk [Row [("id",PgInt 1)]]) (AccessOutOfBound (LList [LInt 1]) (LInt 2))
            ]
          )

    withClient (mkApp defaultEnv) $
      it "succeed" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- *** succeed: access rich table


  describe "succeed: access rich table" $ do
    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes =
              [ buildPgSceneWithScript
                []
                [ AssertEqual
                    (LAccessOp (LAccessOp LPgRichResponse (LInt 0)) (LString "id"))
                    (LInt 1)
                ]
                (defaultPgComputationInput { _pgComputationInputSql = [ Sentence "select 1 as \"id\"" ]})
              ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ mkSceneOutput $ PgSceneOk (PgTuplesOk [ Row [ ("id", PgInt 1) ] ] )
            ]
          )

    withClient (mkApp defaultEnv) $
      it "succeed" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- *** fail: access rich table


  describe "fail: access rich table" $ do
    let (input, output) =
          ( ScenarioInput
            { _scenarioInputId = UUID.nil
            , _scenarioInputScenes =
              [ buildPgSceneWithScript
                []
                [ AssertEqual
                    (LAccessOp (LAccessOp LPgRichResponse (LInt 0)) (LString "name"))
                    (LInt 1)
                ]
                (defaultPgComputationInput { _pgComputationInputSql = [ Sentence "select 1 as \"id\"" ]})
              ]
            , _scenarioInputEnvVars = Map.empty
            }
          , ScenarioOutput
            [ mkSceneOutput $ PgPostscriptFailed (PgTuplesOk [Row [("id",PgInt 1)]]) AssertionFailed LNull (LInt 1) "LNull is not equal to LInt 1"
            ]
          )

    withClient (mkApp defaultEnv) $
      it "succeed" $ \clientEnv ->
        try clientEnv (runScenarioComputation input) `shouldReturn` output


-- * util


-- ** build input scene


-- *** http


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


-- *** pg


buildPgSceneWithScript :: TangoAst -> TangoAst -> PgComputationInput -> SceneFile
buildPgSceneWithScript prescript postscript pgComputationInput =
  PgSceneFile { _sceneId         = UUID.nil
              , _sceneFileId     = UUID.nil
              , _scenePrescript  = prescript
              , _scenePostscript = postscript
              , _scenePgInput    = pgComputationInput
              }

defaultPgComputationInput :: PgComputationInput
defaultPgComputationInput =
  PgComputationInput { _pgComputationInputSql             = [ Sentence "select 1;" ]
                     , _pgComputationInputPgConnection    =
                       TemplatedPgConnection { _templatedPgConnectionHost     = [ Sentence "localhost" ]
                                             , _templatedPgConnectionPort     = [ Sentence "5432" ]
                                             , _templatedPgConnectionUser     = [ Sentence "postgres" ]
                                             , _templatedPgConnectionPassword = [ Sentence "" ]
                                             , _templatedPgConnectionDbName   = [ Sentence "test" ]
                                             }
                     }


-- ** build output scene


mkSceneOutput :: SceneComputation -> SceneOutput
mkSceneOutput sceneComputationOutput =
  SceneOutput { _outputSceneId = UUID.nil
              , _outputSceneRequestFileNodeId = UUID.nil
              , _outputSceneComputation = sceneComputationOutput
              }


-- ** build response


requestComputation :: RequestComputation
requestComputation =
  RequestComputation { _requestComputationStatusCode = 200
                     , _requestComputationHeaders    = []
                     , _requestComputationBody       = ""
                     }
