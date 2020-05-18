{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators             #-}

module InterpolatorSpec where

import qualified Data.Map.Strict as Map
import           Interpolator
import           Test.Hspec


-- * spec


spec :: Spec
spec =
    describe "interpolation" $ do
      it "doesnt interpolate when there is no key" $ do
        let envVars = Map.fromList []
        interpolate envVars [ Sentence "hello ", Sentence "user" ]
          `shouldBe` "hello user"

      it "interpolate existing key" $ do
        let envVars = Map.fromList [("user", [Sentence "John"])]
        interpolate envVars [ Sentence "hello ", Key "user" ]
          `shouldBe` "hello John"

      it "interpolate multiple keys" $ do
        let envVars = Map.fromList [ ("fname", [Sentence "John"])
                                   , ("lname", [Sentence "Doe"])
                                   ]
        interpolate envVars [ Sentence "hello "
                            , Key "fname"
                            , Sentence " "
                            , Key "lname"
                            , Sentence "!"
                            ]
          `shouldBe` "hello John Doe!"

      it "doesn't interpolate missing key" $ do
        let envVars = Map.fromList [ ("fname", [Sentence "John"])
                                   ]
        interpolate envVars [ Sentence "hello "
                            , Key "fname"
                            , Sentence " "
                            , Key "lname"
                            , Sentence "!"
                            ]
          `shouldBe` "hello John {{lname}}!"
