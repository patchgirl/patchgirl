{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

module ScenarioComputation.App ( runScenarioComputationHandler) where


import qualified Control.Monad             as Monad
import qualified Control.Monad.IO.Class    as IO
import qualified Control.Monad.Reader      as Reader
import           Data.Functor              ((<&>))
import           PatchGirl
import           RequestComputation.App
import           RequestComputation.Model
import           ScenarioComputation.Model


-- * run scenario computation handler


runScenarioComputationHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => ScenarioComputationInput
  -> m ScenarioComputationOutput
runScenarioComputationHandler (ScenarioComputationInput inputScenario) =
  buildOutputScenario inputScenario <&> ScenarioComputationOutput

buildOutputScenario :: (Reader.MonadReader Env m, IO.MonadIO m) => InputScenario -> m OutputScenario
buildOutputScenario InputScenario{..} =
  Monad.foldM buildScenes [] _inputScenarioScenes <&> \outputScenes ->
    OutputScenario { _outputScenarioId     = _inputScenarioId
                   , _outputScenarioScenes = outputScenes
                   }
  where
    buildScenes :: (Reader.MonadReader Env m, IO.MonadIO m) => [OutputScene] -> InputScene -> m [OutputScene]
    buildScenes acc inputScene =
      case lastSceneWasSuccessful acc of
        True -> do
          outputScene <- buildOutputScene inputScene
          return $ acc ++ [ outputScene ]
        False ->
          return $ acc ++ [ notRunOutputScene inputScene ]

    lastSceneWasSuccessful :: [OutputScene] -> Bool
    lastSceneWasSuccessful = \case
      [] -> True
      [x] -> case _outputSceneRequestComputationOutput x of
        SceneRun (RequestComputationSucceeded _) -> True
        _                                        -> False
      (_:xs) -> lastSceneWasSuccessful xs

    buildOutputScene :: (Reader.MonadReader Env m, IO.MonadIO m) => InputScene -> m OutputScene
    buildOutputScene inputScene@InputScene{..} =
      case _inputSceneRequestComputationInput of
        Nothing ->
          return $ notRunOutputScene inputScene

        Just requestComputationInput ->
          runRequestComputationHandler requestComputationInput <&> \requestComputationResult ->
            OutputScene { _outputSceneId = _inputSceneId
                        , _outputSceneRequestFileNodeId = _inputSceneRequestFileNodeId
                        , _outputSceneRequestComputationOutput = SceneRun requestComputationResult
                        }

    notRunOutputScene :: InputScene -> OutputScene
    notRunOutputScene InputScene{..} =
      OutputScene { _outputSceneId = _inputSceneId
                    , _outputSceneRequestFileNodeId = _inputSceneRequestFileNodeId
                    , _outputSceneRequestComputationOutput = SceneNotRun
                    }
