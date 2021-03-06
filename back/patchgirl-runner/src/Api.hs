{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module Api( RunnerApi
          , runnerApiServer
          , RequestComputationApi
          , ScenarioComputationApi
          , PgSqlComputationApi
          ) where

import           Servant                   hiding (BadPassword, NoSuchUser)

import           Health.App
import           Interpolator
import           Model
import           PgSqlComputation.App
import           PgSqlComputation.Model
import           RequestComputation.App
import           RequestComputation.Model
import           ScenarioComputation.App
import           ScenarioComputation.Model


type RunnerApi =
  RequestComputationApi :<|>
  ScenarioComputationApi :<|>
  PgSqlComputationApi :<|>
  HealthApi

runnerApiServer :: ServerT RunnerApi AppM
runnerApiServer =
  requestComputationApiServer
  :<|> scenarioComputationApiServer
  :<|> pgSqlComputationApiServer
  :<|> healthApiServer


-- * request computation


type RequestComputationApi =
  "api" :> "runner" :> "requestComputation" :> (
    ReqBody '[JSON] (TemplatedRequestComputationInput, EnvironmentVars) :> Post '[JSON] RequestComputationOutput
  )

requestComputationApiServer :: (TemplatedRequestComputationInput, EnvironmentVars) -> AppM RequestComputationOutput
requestComputationApiServer =
  runRequestComputationHandler


-- * scenario computation


type ScenarioComputationApi =
  "api" :> "runner" :> "scenarioComputation" :> (
    ReqBody '[JSON] ScenarioInput :> Post '[JSON] ScenarioOutput
  )

scenarioComputationApiServer :: ScenarioInput -> AppM ScenarioOutput
scenarioComputationApiServer =
  runScenarioComputationHandler


-- * pg sql computation


type PgSqlComputationApi =
  "api" :> "runner" :> "pgSqlComputation" :> (
    ReqBody '[JSON] (EnvironmentVars, PgComputationInput) :> Post '[JSON] PgComputationOutput
  )

pgSqlComputationApiServer :: (EnvironmentVars, PgComputationInput) -> AppM PgComputationOutput
pgSqlComputationApiServer =
  runPgSqlComputationHandler


-- * health


type HealthApi =
  "api" :> "runner" :> "health" :> Get '[JSON] ()

healthApiServer :: AppM ()
healthApiServer =
  healthHandler
