{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Api( RunnerApi
          , runnerApiServer
          , RequestComputationApi
          , ScenarioComputationApi
          ) where

import           Servant                   hiding (BadPassword, NoSuchUser)

import           Health.App
import           Interpolator
import           Model
import           RequestComputation.App
import           RequestComputation.Model
import           ScenarioComputation.App
import           ScenarioComputation.Model


-- * runner


type RunnerApi =
  RequestComputationApi :<|>
  ScenarioComputationApi :<|>
  HealthApi

runnerApiServer :: ServerT RunnerApi AppM
runnerApiServer =
  requestComputationApiServer
  :<|> scenarioComputationApiServer
  :<|> healthApiServer


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


-- ** health


type HealthApi =
  "api" :> "runner" :> "health" :> (
    Get '[JSON] ()
  )

healthApiServer :: AppM ()
healthApiServer =
  healthHandler
