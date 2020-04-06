{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}

module ScenarioCollection.AppSpec where

import           Account.DB
import           App
import           Helper.App
import qualified Network.HTTP.Types       as HTTP
import           ScenarioCollection.DB
import           ScenarioCollection.Model
import           Servant
import qualified Servant.Auth.Client      as Auth
import           Servant.Auth.Server      (JWT)
import           Servant.Client
import           Test.Hspec


-- * client


getScenarioCollectionById :: Auth.Token -> ClientM ScenarioCollection
getScenarioCollectionById =
  client (Proxy :: Proxy (PScenarioCollectionApi '[JWT]))


-- * spec


spec :: Spec
spec =
  withClient (mkApp defaultConfig) $

    describe "get scenario collection by id" $ do
      it "returns notFound404 when scenarioCollection does not exist" $ \clientEnv ->
        cleanDBAfter $ \_ -> do
          (token, _) <- signedUserToken1
          try clientEnv (getScenarioCollectionById token) `shouldThrow` errorsWithStatus HTTP.notFound404

      it "returns an empty scenario collection if the account doesnt have a scenario collection" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          accountId <- insertFakeAccount defaultNewFakeAccount1 connection
          scenarioCollectionId <- insertFakeScenarioCollection accountId connection
          token <- signedUserToken accountId
          scenarioCollection <- try clientEnv (getScenarioCollectionById token)
          scenarioCollection `shouldBe` ScenarioCollection scenarioCollectionId []

      it "returns the account's scenario collection" $ \clientEnv ->
        cleanDBAfter $ \connection -> do
          accountId <- insertFakeAccount defaultNewFakeAccount1 connection
          expectedScenarioCollection <- insertSampleScenarioCollection accountId connection
          token <- signedUserToken accountId
          scenarioCollection <- try clientEnv (getScenarioCollectionById token)
          scenarioCollection `shouldBe` expectedScenarioCollection
