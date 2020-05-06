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
  => ScenarioInput
  -> m ScenarioOutput
runScenarioComputationHandler inputScenario =
  buildScenarioOutput inputScenario

buildScenarioOutput :: (Reader.MonadReader Env m, IO.MonadIO m) => ScenarioInput -> m ScenarioOutput
buildScenarioOutput ScenarioInput{..} =
  Monad.foldM buildScenes [] _inputScenarioScenes <&> \outputScenes ->
    ScenarioOutput outputScenes
  where
    buildScenes :: (Reader.MonadReader Env m, IO.MonadIO m) => [SceneOutput] -> SceneInput -> m [SceneOutput]
    buildScenes acc inputScene = do
      scene <- buildScene (lastSceneWasSuccessful acc) _inputScenarioGlobalEnv inputScene
      return $ acc ++ [ scene ]

    lastSceneWasSuccessful :: [SceneOutput] -> Bool
    lastSceneWasSuccessful = \case
      [] -> True
      [x] -> case _outputSceneComputation x of
        SceneSucceeded _ -> True
        _                -> False
      (_:xs) -> lastSceneWasSuccessful xs


-- ** build scene


buildScene :: (Reader.MonadReader Env m, IO.MonadIO m) => Bool -> ScenarioEnvironment -> SceneInput -> m SceneOutput
buildScene lastSceneWasSuccessful globalEnvironment inputScene =
  case (_inputSceneRequestComputationInput inputScene <&> \r -> (lastSceneWasSuccessful, r)) of
    Just (True, requestComputationInput) ->
      let
        prescriptResult = runPrescript globalEnvironment inputScene
        in
        case prescriptResult of
          Left _ ->
            return $ buildSceneOutput inputScene PrescriptFailed

          Right _ -> do
            requestComputationResult <- runRequestComputationHandler requestComputationInput
            return . buildSceneOutput inputScene $
              case requestComputationResult of
                Left httpException -> RequestFailed httpException
                Right requestComputationOutput -> SceneSucceeded requestComputationOutput

    _ ->
      return $ buildSceneOutput inputScene SceneNotRun


-- ** pre script


runPrescript :: ScenarioEnvironment -> SceneInput -> Either () PrescriptOutput
runPrescript globalEnvironment SceneInput{..} =
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

buildSceneOutput :: SceneInput -> SceneComputation -> SceneOutput
buildSceneOutput SceneInput{..} sceneComputation =
  SceneOutput { _outputSceneId = _inputSceneId
              , _outputSceneRequestFileNodeId = _inputSceneRequestFileNodeId
              , _outputSceneComputation = sceneComputation
              }
