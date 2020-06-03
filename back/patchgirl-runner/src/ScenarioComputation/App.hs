{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

module ScenarioComputation.App ( runScenarioComputationHandler) where

import qualified Control.Monad             as Monad
import qualified Control.Monad.IO.Class    as IO
import qualified Control.Monad.Reader      as Reader
import           Data.Functor              ((<&>))
import qualified Data.Map.Strict           as Map

import           Env
import           Interpolator
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
runScenarioComputationHandler ScenarioInput{..} =
  Monad.foldM (buildScenes _scenarioInputEnvVars) (Map.empty, []) _scenarioInputScenes <&> ScenarioOutput . snd
  where
    buildScenes
      :: (Reader.MonadReader Env m, IO.MonadIO m)
      => EnvironmentVars
      -> (ScenarioVars, [SceneOutput])
      -> SceneInput
      -> m (ScenarioVars, [SceneOutput])
    buildScenes environmentVars (scenarioGlobalVars, scenes) inputScene = do
      (scene, newGlobalEnvironment) <- runScene (lastSceneWasSuccessful scenes) environmentVars scenarioGlobalVars inputScene
      return (newGlobalEnvironment, scenes ++ [ scene ])

    lastSceneWasSuccessful :: [SceneOutput] -> Bool
    lastSceneWasSuccessful = \case
      [] -> True
      [x] -> case _outputSceneComputation x of
        SceneSucceeded _ -> True
        _                -> False
      (_:xs) -> lastSceneWasSuccessful xs


-- ** run scene


runScene :: (Reader.MonadReader Env m, IO.MonadIO m) => Bool -> EnvironmentVars -> ScenarioVars -> SceneInput -> m (SceneOutput, ScenarioVars)
runScene lastSceneWasSuccessful environmentVars scenarioGlobalVars sceneInput =
  case lastSceneWasSuccessful of
    True ->
      case runPrescript scenarioGlobalVars Map.empty sceneInput of
        Left scriptException ->
          return ( buildSceneOutput sceneInput (PrescriptFailed scriptException)
                 , scenarioGlobalVars
                 )

        Right (scenarioGlobalVarsAfterPrescript, scenariolocalVarsAfterPrescript) -> do
          requestComputationResult <-
            runRequestComputationWithScenarioContext (_sceneInputTemplatedRequestComputationInput sceneInput) environmentVars scenarioGlobalVarsAfterPrescript scenariolocalVarsAfterPrescript
          case requestComputationResult of
            Left httpException ->
              return ( buildSceneOutput sceneInput (RequestFailed httpException)
                     , scenarioGlobalVars
                     )

            Right requestComputationOutput ->
              case runPostscript scenarioGlobalVarsAfterPrescript Map.empty sceneInput requestComputationOutput of
                Left scriptException ->
                  return ( buildSceneOutput sceneInput (PostscriptFailed scriptException)
                         , scenarioGlobalVarsAfterPrescript
                         )

                Right scenarioGlobalVarsAfterPostscript ->
                  return ( buildSceneOutput sceneInput (SceneSucceeded requestComputationOutput)
                         , scenarioGlobalVarsAfterPostscript
                         )

    _ ->
      return ( buildSceneOutput sceneInput SceneNotRun
             , scenarioGlobalVars
             )


-- ** pre script


runPrescript :: ScenarioVars -> ScenarioVars -> SceneInput -> Either ScriptException (ScenarioVars, ScenarioVars)
runPrescript scenarioGlobalVars scenarioLocalVars SceneInput{..} =
  foldl f (Right (scenarioGlobalVars, scenarioLocalVars)) _sceneInputPrescript
  where
    f
      :: Either ScriptException (ScenarioVars, ScenarioVars)
      -> Proc
      -> Either ScriptException (ScenarioVars, ScenarioVars)
    f acc proc =
      acc >>= \env -> runPrescriptProc env proc


-- ** run post script


runPostscript
  :: ScenarioVars
  -> ScenarioVars
  -> SceneInput
  -> RequestComputationOutput
  -> Either ScriptException ScenarioVars
runPostscript scenarioGlobalVars scenarioLocalVars SceneInput{..} requestComputationOutput =
  foldl f (Right (scenarioGlobalVars, scenarioLocalVars)) _sceneInputPostscript <&> fst
  where
    f
      :: Either ScriptException (ScenarioVars, ScenarioVars)
      -> Proc
      -> Either ScriptException (ScenarioVars, ScenarioVars)
    f acc proc =
      acc >>= \env -> runPostscriptProc requestComputationOutput env proc


-- ** run postscript proc


runPostscriptProc
  :: RequestComputationOutput
  -> (ScenarioVars, ScenarioVars)
  -> Proc
  -> Either ScriptException (ScenarioVars, ScenarioVars)
runPostscriptProc requestComputationOutput (scenarioGlobalVars, scenarioLocalVars) = \case
  AssertEqual expr1 expr2 ->
    let
      ex1 = runPostscriptExpr requestComputationOutput scenarioGlobalVars scenarioLocalVars expr1
      ex2 = runPostscriptExpr requestComputationOutput scenarioGlobalVars scenarioLocalVars expr2
    in
      case (ex1, ex2) of
        (Just a, Just b) ->
          case a == b of
            True  -> Right (scenarioGlobalVars, scenarioLocalVars)
            False -> Left (AssertEqualFailed a b)

        (Nothing, _) ->
          Left (UnknownVariable expr1)

        (_, Nothing) ->
          Left (UnknownVariable expr2)

  Let var expr ->
    case runPostscriptExpr requestComputationOutput scenarioGlobalVars scenarioLocalVars expr of
      Just newExpr ->
        let newLocalEnvironment = Map.insert var newExpr scenarioLocalVars
        in Right (scenarioGlobalVars, newLocalEnvironment)

      _ ->
        Left (UnknownVariable expr)

  Set var expr ->
    case runPostscriptExpr requestComputationOutput scenarioGlobalVars scenarioLocalVars expr of
      Just newExpr ->
        let newGlobalEnvironment = Map.insert var newExpr scenarioGlobalVars
        in Right (newGlobalEnvironment, scenarioLocalVars)

      _ ->
        Left (UnknownVariable expr)


-- ** run prescript proc


runPrescriptProc
  :: (ScenarioVars, ScenarioVars)
  -> Proc
  -> Either ScriptException (ScenarioVars, ScenarioVars)
runPrescriptProc (scenarioGlobalVars, scenarioLocalVars) = \case
  AssertEqual expr1 expr2 ->
    let
      ex1 = runPrescriptExpr scenarioGlobalVars scenarioLocalVars expr1
      ex2 = runPrescriptExpr scenarioGlobalVars scenarioLocalVars expr2
    in
      case (ex1, ex2) of
        (Just a, Just b) ->
          case a == b of
            True  -> Right (scenarioGlobalVars, scenarioLocalVars)
            False -> Left (AssertEqualFailed a b)

        (Nothing, _) ->
          Left (UnknownVariable expr1)

        (_, Nothing) ->
          Left (UnknownVariable expr2)

  Let var expr ->
    case runPrescriptExpr scenarioGlobalVars scenarioLocalVars expr of
      Just newExpr ->
        let newLocalEnvironment = Map.insert var newExpr scenarioLocalVars
        in Right (scenarioGlobalVars, newLocalEnvironment)

      _ ->
        Left (UnknownVariable expr)

  Set var expr ->
    case runPrescriptExpr scenarioGlobalVars scenarioLocalVars expr of
      Just newExpr ->
        let newGlobalEnvironment = Map.insert var newExpr scenarioGlobalVars
        in Right (newGlobalEnvironment, scenarioLocalVars)

      _ ->
        Left (UnknownVariable expr)


-- ** run postscript expr


runPostscriptExpr
  :: RequestComputationOutput
  -> ScenarioVars
  -> ScenarioVars
  -> Expr
  -> Maybe Expr
runPostscriptExpr RequestComputationOutput{..} scenarioGlobalVars scenarioLocalVars = \case
  Var var -> Map.lookup var scenarioLocalVars
  Fetch var -> Map.lookup var scenarioGlobalVars
  HttpResponseBodyAsString -> Just (LString _requestComputationOutputBody)
  HttpResponseStatus -> Just (LInt _requestComputationOutputStatusCode)
  expr -> Just expr


-- ** run prescript expr


runPrescriptExpr :: ScenarioVars -> ScenarioVars -> Expr -> Maybe Expr
runPrescriptExpr scenarioGlobalVars scenarioLocalVars = \case
  Var var -> Map.lookup var scenarioLocalVars
  Fetch var -> Map.lookup var scenarioGlobalVars
  HttpResponseBodyAsString -> Nothing
  expr -> Just expr


-- ** scene


buildSceneOutput :: SceneInput -> SceneComputation -> SceneOutput
buildSceneOutput SceneInput{..} sceneComputation =
  SceneOutput { _outputSceneId = _sceneInputId
              , _outputSceneRequestFileNodeId = _sceneInputRequestFileNodeId
              , _outputSceneComputation = sceneComputation
              }
