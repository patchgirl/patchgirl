{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

module ScenarioComputation.App ( runScenarioComputationHandler) where


--import qualified Control.Monad.IO.Class    as IO
--import qualified Control.Monad.Reader      as Reader
--import           PatchGirl
import           ScenarioComputation.Model


-- * handler


runScenarioComputationHandler
--  :: ( Reader.MonadReader Env m
--     , IO.MonadIO m
--     )
--  => ScenarioComputationInput
  :: ScenarioComputationInput
  -> m ScenarioComputationOutput
runScenarioComputationHandler _ =
  undefined
