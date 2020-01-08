{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Session.AppSpec where

import           App
import           Test.Hspec
import Helper.DB (cleanDBAfter)
import Helper.App

spec :: Spec
spec = do
  describe "GET /requestCollection/:id/requestNodes" $ do
    it "true" $ do
      True `shouldBe` True
