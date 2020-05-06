{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

module ScenarioComputation.App ( runScenarioComputationHandler) where


import qualified Control.Monad             as Monad
import qualified Control.Monad.IO.Class    as IO
import qualified Control.Monad.Reader      as Reader
import           Data.Functor              ((<&>))
import qualified Data.Map.Strict           as Map

import           PatchGirl
import           RequestComputation.App
import           RequestComputation.Model
import           ScenarioComputation.Model
import           TangoScript


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
      case (lastSceneWasSuccessful acc, _inputSceneRequestComputationInput inputScene) of
        (False, _) ->
          return $ acc ++ [ buildOutputScene inputScene SceneNotRun ]

        (_, Nothing) ->
          return $ acc ++ [ buildOutputScene inputScene SceneNotRun ]

        (True, Just requestComputationInput) ->
          let
            prescriptResult = runPrescript _inputScenarioGlobalEnv inputScene
            in
            case prescriptResult of
              Left _ ->
                return $ acc ++ [ buildOutputScene inputScene PrescriptFailed ]

              Right prescriptOutput -> do
                requestComputationResult <- runRequestComputationHandler requestComputationInput
                case requestComputationResult of
                  Left httpException ->
                    return $ acc ++ [ buildOutputScene inputScene (RequestFailed httpException) ]
                  Right requestComputationOutput ->
                    return $ acc ++ [ buildOutputScene inputScene (SceneSucceeded requestComputationOutput) ]

    lastSceneWasSuccessful :: [OutputScene] -> Bool
    lastSceneWasSuccessful = \case
      [] -> True
      [x] -> case _outputSceneComputation x of
        SceneSucceeded _ -> True
        _                -> False
      (_:xs) -> lastSceneWasSuccessful xs


-- ** build scene


buildScene :: (Reader.MonadReader Env m, IO.MonadIO m) => Bool -> ScenarioEnvironment -> InputScene -> m OutputScene
buildScene lastSceneWasSuccessful globalEnvironment inputScene =
  case Just lastSceneWasSuccessful >>= const (_inputSceneRequestComputationInput inputScene) of
    Nothing ->
      return $ buildOutputScene inputScene SceneNotRun

    Just requestComputationInput ->
      let
        prescriptResult = runPrescript globalEnvironment inputScene
        in
        case prescriptResult of
          Left _ ->
            return $ buildOutputScene inputScene PrescriptFailed

          Right prescriptOutput -> do
            requestComputationResult <- runRequestComputationHandler requestComputationInput
            case requestComputationResult of
              Left httpException ->
                return $ buildOutputScene inputScene (RequestFailed httpException)
              Right requestComputationOutput ->
                return $ buildOutputScene inputScene (SceneSucceeded requestComputationOutput)


-- ** pre script


runPrescript :: ScenarioEnvironment -> InputScene -> Either () PrescriptOutput
runPrescript globalEnvironment InputScene{..} =
  foldl f init _inputScenePreScript
  where
    init :: Either () PrescriptOutput
    init =
      Right PrescriptOutput { _outputPrescriptNewGlobalEnvironment = globalEnvironment
                            , _outputPrescriptNewLocalEnvironment = Map.fromList []
                            }

    f :: Either () PrescriptOutput -> Proc -> Either () PrescriptOutput
    f acc proc =
      acc >>= \PrescriptOutput{..} -> runProc _outputPrescriptNewGlobalEnvironment _outputPrescriptNewLocalEnvironment proc

runProc :: ScenarioEnvironment -> ScenarioEnvironment -> Proc -> Either () PrescriptOutput
runProc globalEnvironment localEnvironment = \case
  AssertEqual expr1 expr2 ->
    Left ()

  Let var expr ->
    let
      _outputPrescriptNewGlobalEnvironment = globalEnvironment
      _outputPrescriptNewLocalEnvironment = Map.insert var expr localEnvironment
      in Right PrescriptOutput{..}

  Set var expr ->
    let
      _outputPrescriptNewGlobalEnvironment = Map.insert var expr globalEnvironment
      _outputPrescriptNewLocalEnvironment = localEnvironment
      in Right PrescriptOutput{..}


-- ** scene

buildOutputScene :: InputScene -> SceneComputation -> OutputScene
buildOutputScene InputScene{..} sceneComputation =
  OutputScene { _outputSceneId = _inputSceneId
              , _outputSceneRequestFileNodeId = _inputSceneRequestFileNodeId
              , _outputSceneComputation = sceneComputation
              }
