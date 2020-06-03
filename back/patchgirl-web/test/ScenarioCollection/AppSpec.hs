{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module ScenarioCollection.AppSpec where

import qualified Network.HTTP.Types  as HTTP
import           Servant
import qualified Servant.Auth.Client as Auth
import           Servant.Auth.Server (JWT)
import           Servant.Client
import           Test.Hspec

import           DBUtil
import           Helper.App
import           PatchGirl.Client
import           PatchGirl.Server


-- * client


getScenarioCollectionById :: Auth.Token -> ClientM ScenarioCollection
getScenarioCollectionById =
  client (Proxy :: Proxy (ScenarioCollectionApi '[JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultEnv) $

    describe "get scenario collection by id" $ do
      it "returns notFound404 when scenarioCollection does not exist" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { token } ->
          try clientEnv (getScenarioCollectionById token) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns an empty scenario collection if the account doesnt have a scenario collection" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          scenarioCollectionId <- insertFakeScenarioCollection accountId connection
          scenarioCollection <- try clientEnv (getScenarioCollectionById token)
          scenarioCollection `shouldBe` ScenarioCollection scenarioCollectionId []

      it "returns the account's scenario collection" $ \clientEnv ->
        createAccountAndcleanDBAfter $ \Test { connection, accountId, token } -> do
          (_, expectedScenarioCollection) <- insertSampleScenarioCollection accountId connection
          scenarioCollection <- try clientEnv (getScenarioCollectionById token)
          scenarioCollection `shouldBe` expectedScenarioCollection
