module TangoScript.AppSpec where

import qualified Control.Monad.State       as State
import qualified Data.Map.Strict           as Map
import           Test.Hspec

import           Interpolator
import           RequestComputation.Model
import           ScenarioComputation.Model
import           TangoScript.App
import           TangoScript.Model


spec :: Spec
spec = do

  describe "reduce to primitive" $ do


-- * list


      it "reduce" $ do
        let input = LList [ LList [ LInt 1 ] ]
        let output = LList [ LList [ LInt 1 ] ]
        reduce input `shouldBe` Right output


-- * truthy/falsy


      it "reduce" $ do
        let input = LEq (LInt 1) (LInt 2)
        let output = LBool False
        reduce input `shouldBe` Right output

      it "reduce" $ do
        let input = LEq (LInt 1) (LInt 1)
        let output = LBool True
        reduce input `shouldBe` Right output


-- * access operator


-- ** normal string


      it "reduce" $ do
        let input = LAccessOp (LString "abcd") (LInt 1)
        let output = LString "b"
        reduce input `shouldBe` Right output


-- ** index too big on string


      it "reduce" $ do
        let input = LAccessOp (LString "abcd") (LInt 10)
        let output = AccessOutOfBound (LString "abcd") (LInt 10)
        reduce input `shouldBe` Left output


-- ** list


      it "reduce" $ do
        let input = LAccessOp (LList [ LInt 0, LInt 1, LInt 2 ]) (LInt 1)
        let output = LInt 1
        reduce input `shouldBe` Right output


-- ** var


      it "reduce" $ do
        let input = LAccessOp (LVar "foo") (LInt 1)
        let output = LString "a"
        let reduced =
              reduceWithScenarioContext input $ emptyScenarioContext { _scenarioContextLocalVars = ScenarioVars $ Map.fromList [ ("foo", LString "bar") ] }
        reduced `shouldBe` Right output


-- ** json array


      it "reduce" $ do
        let input = LAccessOp (LJson (JArray [JInt 1, JInt 2, JInt 3])) (LInt 1)
        let output = LInt 2
        reduce input `shouldBe` Right output


-- ** json object


      it "reduce" $ do
        let input = LAccessOp (LJson (JObject $ Map.fromList [ ("foo", JString "bar"), ("baz", JString "biz") ] )) (LString "foo")
        let output = LString "bar"
        reduce input `shouldBe` Right output


-- ** missing json key


      it "reduce" $ do
        let input = LAccessOp (LJson (JObject $ Map.fromList [ ("foo", JString "bar"), ("baz", JString "biz") ] )) (LString "toto")
        let output = AccessOutOfBound (LJson (JObject (Map.fromList [("baz",JString "biz"),("foo",JString "bar")]))) (LString "toto")
        reduce input `shouldBe` Left output


-- ** json string


      it "reduce" $ do
        let input = LAccessOp (LJson (JString "abc")) (LInt 1)
        let output = LString "b"
        reduce input `shouldBe` Right output


-- ** http json response


      it "reduce" $ do
        let input = LAccessOp LHttpResponseBodyAsJson (LString "foo")
        let output = LString "bar"
        let reduced = reduceWithFullContext input emptyScenarioContext (PostScene $ mkRequestComputationFromBody " { \"foo\": \"bar\"  } ")
        reduced `shouldBe` Right output


-- * util


  where
    reduce :: Expr -> Either ScriptException Expr
    reduce expr =
      reduceWithScenarioContext expr emptyScenarioContext

    reduceWithScenarioContext :: Expr -> ScenarioContext -> Either ScriptException Expr
    reduceWithScenarioContext expr scenarioContext =
      reduceWithFullContext expr scenarioContext PreScene

    reduceWithFullContext :: Expr -> ScenarioContext -> Context a -> Either ScriptException Expr
    reduceWithFullContext expr scenarioContext context =
      State.evalState (reduceExprToPrimitive context expr) scenarioContext

    mkRequestComputationFromBody :: String -> RequestComputation
    mkRequestComputationFromBody body =
      RequestComputation { _requestComputationRequestHeaders = []
                         , _requestComputationRequestBody = ""
                         , _requestComputationResponseStatusCode = 200
                         , _requestComputationResponseHeaders    = []
                         , _requestComputationResponseBody       = body
                         }
