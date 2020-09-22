{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module InterpolatorSpec where

import qualified Data.Map.Strict as Map

import           Interpolator
import           TangoScript.Model
import           Test.Hspec


-- * spec


spec :: Spec
spec =
    describe "interpolation" $ do
      it "doesnt interpolate when there is no key" $ do
        let envVars = Map.empty
        let scenarioVars = Map.empty
        interpolate envVars scenarioVars scenarioVars [ Sentence "hello ", Sentence "user" ]
          `shouldBe` "hello user"

      it "interpolate existing key" $ do
        let envVars = Map.fromList [("user", [Sentence "John"])]
        let scenarioVars = Map.empty
        interpolate envVars scenarioVars scenarioVars [ Sentence "hello ", Key "user" ]
          `shouldBe` "hello John"

      it "interpolate multiple keys" $ do
        let envVars = Map.fromList [ ("fname", [Sentence "John"])
                                   , ("lname", [Sentence "Doe"])
                                   ]
        let scenarioVars = Map.empty
        interpolate envVars scenarioVars scenarioVars [ Sentence "hello "
                                                      , Key "fname"
                                                      , Sentence " "
                                                      , Key "lname"
                                                      , Sentence "!"
                                                      ]
          `shouldBe` "hello John Doe!"

      it "doesn't interpolate missing key" $ do
        let envVars = Map.fromList [ ("fname", [Sentence "John"])
                                   ]
        let scenarioVars = Map.empty
        interpolate envVars scenarioVars scenarioVars [ Sentence "hello "
                                                      , Key "fname"
                                                      , Sentence " "
                                                      , Key "lname"
                                                      , Sentence "!"
                                                      ]
          `shouldBe` "hello John {{lname}}!"

      it "interpolate with env first then global vars then local vars" $ do
        let envVars = Map.fromList [ ("a", [Sentence "env a"])
                                   ]
        let scenarioGlobalVars = Map.fromList [ ("a", LString "global a")
                                              , ("b", LString "global b")
                                              ]
        let scenariolocalVars = Map.fromList [ ("a", LString "local a")
                                             , ("b", LString "local b")
                                             , ("c", LString "local c")
                                              ]
        interpolate envVars scenarioGlobalVars scenariolocalVars [ Key "a"
                                                                 , Sentence " - "
                                                                 , Key "b"
                                                                 , Sentence " - "
                                                                 , Key "c"
                                                                 ]
          `shouldBe` "env a - global b - local c"
