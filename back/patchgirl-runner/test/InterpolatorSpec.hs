{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module InterpolatorSpec where

import qualified Data.Map.Strict                  as Map

import           Interpolator
import qualified PatchGirl.Web.ScenarioNode.Model as Web
import           TangoScript.Model
import           Test.Hspec


-- * spec


spec :: Spec
spec =
    describe "interpolation" $ do
      it "doesnt interpolate when there is no key" $ do
        let envVars = Map.empty
        let scenarioVars = Map.empty
        interpolate Web.emptySceneVariable envVars scenarioVars scenarioVars [ Sentence "hello ", Sentence "user" ]
          `shouldBe` "hello user"

      it "interpolate existing key" $ do
        let envVars = Map.fromList [("user", [Sentence "John"])]
        let scenarioVars = Map.empty
        interpolate Web.emptySceneVariable envVars scenarioVars scenarioVars [ Sentence "hello ", Key "user" ]
          `shouldBe` "hello John"

      it "interpolate multiple keys" $ do
        let envVars = Map.fromList [ ("fname", [Sentence "John"])
                                   , ("lname", [Sentence "Doe"])
                                   ]
        let scenarioVars = Map.empty
        interpolate Web.emptySceneVariable envVars scenarioVars scenarioVars [ Sentence "hello "
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
        interpolate Web.emptySceneVariable envVars scenarioVars scenarioVars [ Sentence "hello "
                                                      , Key "fname"
                                                      , Sentence " "
                                                      , Key "lname"
                                                      , Sentence "!"
                                                      ]
          `shouldBe` "hello John {{lname}}!"

      it "interpolate with scene vars first then local vars then global vars then environment vars" $ do
        let envVars = Map.fromList [ ("a", [Sentence "env a"])
                                   , ("b", [Sentence "env b"])
                                   , ("c", [Sentence "env c"])
                                   , ("d", [Sentence "env d"])
                                   ]
        let scenarioGlobalVars = Map.fromList [ ("a", LString "global a")
                                              , ("b", LString "global b")
                                              , ("c", LString "global c")
                                              ]
        let scenariolocalVars = Map.fromList [ ("a", LString "local a")
                                             , ("b", LString "local b")
                                             ]
        let sceneVars =
              Web.SceneVariables $ Map.fromList [ ( "a"
                                                  , Web.SceneVariableValue { _sceneVariableValueValue   = "scene a"
                                                                           , _sceneVariableValueEnabled = True
                                                                           }
                                                  )
                                                ]

        interpolate sceneVars envVars scenarioGlobalVars scenariolocalVars [ Key "a"
                                                                           , Sentence " - "
                                                                           , Key "b"
                                                                           , Sentence " - "
                                                                           , Key "c"
                                                                           , Sentence " - "
                                                                           , Key "d"
                                                                           ]
          `shouldBe` "scene a - local b - global c - env d"

      it "interpolate with scene vars if the scene var is enabled" $ do
        let scenariolocalVars = Map.fromList [ ("a", LString "local a")
                                             , ("b", LString "local b")
                                             ]
        let sceneVars =
              Web.SceneVariables $ Map.fromList [ ( "a"
                                                  , Web.SceneVariableValue { _sceneVariableValueValue   = "scene a"
                                                                           , _sceneVariableValueEnabled = True
                                                                           }
                                                  )
                                                , ( "b"
                                                  , Web.SceneVariableValue { _sceneVariableValueValue   = "scene a"
                                                                           , _sceneVariableValueEnabled = False
                                                                           }
                                                  )
                                                ]

        interpolate sceneVars Map.empty Map.empty scenariolocalVars [ Key "a"
                                                                    , Sentence " - "
                                                                    , Key "b"
                                                                    ]
          `shouldBe` "scene a - local b"
