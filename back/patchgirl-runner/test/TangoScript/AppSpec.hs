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

      it "reduces List of expr" $ do
        let input =
              LList [ LList [ LInt 1 ] ]
        let output =
              LList [ LList [ LInt 1 ] ]

        reduce input `shouldBe` Right output

      it "reduces simple falsy LEq" $ do
        let input =
              LEq (LInt 1) (LInt 2)
        let output =
              LBool False

        reduce input `shouldBe` Right output

      it "reduces simple truthy LEq" $ do
        let input =
              LEq (LInt 1) (LInt 1)
        let output =
              LBool True

        reduce input `shouldBe` Right output

  where
    reduce :: Expr -> Either ScriptException Expr
    reduce expr =
      State.evalState (reduceExprToPrimitive PreScene expr) $ ScriptContext Map.empty Map.empty Map.empty
