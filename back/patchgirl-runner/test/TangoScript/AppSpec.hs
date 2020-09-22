module TangoScript.AppSpec where

import           Test.Hspec
import qualified Control.Monad.State       as State
import qualified Data.Map.Strict           as Map

import TangoScript.Model
import TangoScript.App
import           ScenarioComputation.Model

spec :: Spec
spec = focus $ do

  describe "reduce to primitive" $ do


-- * list


      it "reduces List of expr" $ do
        let input = LList [ LList [ LInt 1 ] ]
        let output = LList [ LList [ LInt 1 ] ]
        reduce input `shouldBe` Right output


-- * truthy/falsy


      it "reduces simple falsy LEq" $ do
        let input = LEq (LInt 1) (LInt 2)
        let output = LBool False
        reduce input `shouldBe` Right output

      it "reduces simple truthy LEq" $ do
        let input = LEq (LInt 1) (LInt 1)
        let output = LBool True
        reduce input `shouldBe` Right output


-- * access operator


-- ** normal string


      it "reduces simple truthy LEq" $ do
        let input = LAccessOp (LString "abcd") (LInt 1)
        let output = LString "b"
        reduce input `shouldBe` Right output


-- ** index too big on string

      it "reduces simple truthy LEq" $ do
        let input = LAccessOp (LString "abcd") (LInt 10)
        let output = AccessOutOfBound (LString "abcd") (LInt 10)
        reduce input `shouldBe` Left output

-- ** list

      it "reduces simple truthy LEq" $ do
        let input = LAccessOp (LList [ LInt 0, LInt 1, LInt 2 ]) (LInt 1)
        let output = LInt 1
        reduce input `shouldBe` Right output

-- ** var


      it "reduce" $ do
        let input = LAccessOp (LVar "foo") (LInt 1)
        let output = LString "a"
        let reduced = reduceWithContext input $ ScriptContext Map.empty Map.empty (Map.fromList [ ("foo", LString "bar") ])
        reduced `shouldBe` Right output

-- ** json array

      it "reduce" $ do
        let input = LAccessOp (LJson (JArray [JInt 1, JInt 2, JInt 3])) (LInt 1)
        let output = LJson $ JInt 2
        reduce input `shouldBe` Right output

-- ** json object


      it "reduce" $ do
        let input = LAccessOp (LJson (JObject $ Map.fromList [ ("foo", JString "bar"), ("baz", JString "biz") ] )) (LString "foo")
        let output = LJson $ JString "bar"
        reduce input `shouldBe` Right output


-- ** json string


      it "reduces" $ do
        let input = LAccessOp (LJson (JString "abc")) (LInt 1)
        let output = LString "b"
        reduce input `shouldBe` Right output


  where
    reduce :: Expr -> Either ScriptException Expr
    reduce expr =
      reduceWithContext expr $ ScriptContext Map.empty Map.empty Map.empty

    reduceWithContext :: Expr -> ScriptContext -> Either ScriptException Expr
    reduceWithContext expr scriptContext =
      State.evalState (reduceExprToPrimitive PreScene expr) scriptContext
