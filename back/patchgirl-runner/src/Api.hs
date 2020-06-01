{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Api( RunnerApi
          , runnerApiServer
          , RequestComputationApi
          , ScenarioComputationApi
          ) where

import           Servant                   hiding (BadPassword, NoSuchUser)

import           Interpolator
import           Model
import           RequestComputation.App
import           RequestComputation.Model
import           ScenarioComputation.App
import           ScenarioComputation.Model


-- * runner


type RunnerApi =
  RequestComputationApi :<|>
  ScenarioComputationApi

runnerApiServer :: ServerT (RunnerApi) AppM
runnerApiServer =
  requestComputationApiServer
  :<|> scenarioComputationApiServer


-- ** request computation


type RequestComputationApi =
  "api" :> "runner" :> "requestComputation" :> (
    ReqBody '[JSON] (TemplatedRequestComputationInput, EnvironmentVars) :> Post '[JSON] RequestComputationResult
  )

requestComputationApiServer :: (TemplatedRequestComputationInput, EnvironmentVars) -> AppM RequestComputationResult
requestComputationApiServer =
  runRequestComputationHandler


-- ** scenario computation


type ScenarioComputationApi =
  "api" :> "runner" :> "scenarioComputation" :> (
    ReqBody '[JSON] ScenarioInput :> Post '[JSON] ScenarioOutput
  )

scenarioComputationApiServer :: ScenarioInput -> AppM ScenarioOutput
scenarioComputationApiServer =
  runScenarioComputationHandler
