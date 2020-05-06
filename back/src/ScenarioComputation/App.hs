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
  Monad.foldM buildScenes (_inputScenarioGlobalEnv, []) _inputScenarioScenes <&> ScenarioOutput . snd
  where
    buildScenes
      :: (Reader.MonadReader Env m, IO.MonadIO m)
      => (ScenarioEnvironment, [SceneOutput])
      -> SceneInput
      -> m (ScenarioEnvironment, [SceneOutput])
    buildScenes (globalEnvironment, scenes) inputScene = do
      (scene, newGlobalEnvironment) <- buildScene (lastSceneWasSuccessful scenes) globalEnvironment inputScene
      return (newGlobalEnvironment, scenes ++ [ scene ])

    lastSceneWasSuccessful :: [SceneOutput] -> Bool
    lastSceneWasSuccessful = \case
      [] -> True
      [x] -> case _outputSceneComputation x of
        SceneSucceeded _ -> True
        _                -> False
      (_:xs) -> lastSceneWasSuccessful xs


-- ** build scene


buildScene :: (Reader.MonadReader Env m, IO.MonadIO m) => Bool -> ScenarioEnvironment -> SceneInput -> m (SceneOutput, ScenarioEnvironment)
buildScene lastSceneWasSuccessful globalEnvironment inputScene =
  case _inputSceneRequestComputationInput inputScene <&> \r -> (lastSceneWasSuccessful, r) of
    Just (True, requestComputationInput) ->
      case runPrescript globalEnvironment (Map.fromList []) inputScene of
        Left prescriptException ->
          return ( buildSceneOutput inputScene (PrescriptFailed prescriptException)
                 , globalEnvironment
                 )

        Right newGlobalEnvironment -> do
          requestComputationResult <- runRequestComputationHandler requestComputationInput
          case requestComputationResult of
            Left httpException ->
              return ( buildSceneOutput inputScene (RequestFailed httpException)
                     , globalEnvironment
                     )

            Right requestComputationOutput ->
              return ( buildSceneOutput inputScene (SceneSucceeded requestComputationOutput)
                     , newGlobalEnvironment
                     )

    _ ->
      return ( buildSceneOutput inputScene SceneNotRun
             , globalEnvironment
             )


-- ** pre script


runPrescript :: ScenarioEnvironment -> ScenarioEnvironment -> SceneInput -> Either PrescriptException ScenarioEnvironment
runPrescript globalEnvironment localEnvironment SceneInput{..} =
  foldl f (Right (globalEnvironment, localEnvironment)) _inputScenePreScript <&> fst
  where
    f
      :: Either PrescriptException (ScenarioEnvironment, ScenarioEnvironment)
      -> Proc
      -> Either PrescriptException (ScenarioEnvironment, ScenarioEnvironment)
    f acc proc =
      acc >>= \env -> runProc env proc

runProc
  :: (ScenarioEnvironment, ScenarioEnvironment)
  -> Proc
  -> Either PrescriptException (ScenarioEnvironment, ScenarioEnvironment)
runProc (globalEnvironment, localEnvironment) = \case
  AssertEqual expr1' expr2' ->
    let mEqual = mapM (runExpr globalEnvironment localEnvironment) (expr1', expr2') <&> uncurry (==)
    in case mEqual of
      Just True -> Right (globalEnvironment, localEnvironment)
      _         -> Left (AssertEqualFailed expr1' expr2')

  Let var expr ->
    case runExpr globalEnvironment localEnvironment expr of
      Just newExpr ->
        let newLocalEnvironment = Map.insert var newExpr localEnvironment
        in Right (globalEnvironment, newLocalEnvironment)

      _ ->
        Left (UnknownVariable expr)

  Set var expr ->
    case runExpr globalEnvironment localEnvironment expr of
      Just newExpr ->
        let newGlobalEnvironment = Map.insert var newExpr globalEnvironment
        in Right (newGlobalEnvironment, localEnvironment)

      _ ->
        Left (UnknownVariable expr)

runExpr :: ScenarioEnvironment -> ScenarioEnvironment -> Expr -> Maybe Expr
runExpr globalEnvironment localEnvironment = \case
  Var var -> Map.lookup var localEnvironment
  Get var -> Map.lookup var globalEnvironment
  expr -> Just expr


-- ** scene

buildSceneOutput :: SceneInput -> SceneComputation -> SceneOutput
buildSceneOutput SceneInput{..} sceneComputation =
  SceneOutput { _outputSceneId = _inputSceneId
              , _outputSceneRequestFileNodeId = _inputSceneRequestFileNodeId
              , _outputSceneComputation = sceneComputation
              }
