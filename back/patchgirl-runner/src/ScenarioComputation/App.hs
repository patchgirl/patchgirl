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
import           PgSqlComputation.App
import           RequestComputation.App
import           RequestComputation.Model
import           ScenarioComputation.Model
import           TangoScript


-- * handler


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
      -> SceneFile
      -> m (ScenarioVars, [SceneOutput])
    buildScenes environmentVars (scenarioGlobalVars, scenes) inputScene = do
      (scene, newGlobalEnvironment) <- runScene (lastSceneWasSuccessful scenes) environmentVars scenarioGlobalVars inputScene
      return (newGlobalEnvironment, scenes ++ [ scene ])

    lastSceneWasSuccessful :: [SceneOutput] -> Bool
    lastSceneWasSuccessful = \case
      [] -> True
      [x] -> case _outputSceneComputation x of
        HttpSceneOk _ -> True
        PgSceneOk _   -> True
        _             -> False
      (_:xs) -> lastSceneWasSuccessful xs


-- * run scene


runScene
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => Bool
  -> EnvironmentVars
  -> ScenarioVars
  -> SceneFile
  -> m (SceneOutput, ScenarioVars)
runScene lastSceneWasSuccessful environmentVars scenarioGlobalVars scene =
  case lastSceneWasSuccessful of
    False -> return (buildSceneOutput scene SceneNotRun, scenarioGlobalVars)
    True ->
      case runPrescript scenarioGlobalVars Map.empty scene of
        Left scriptException ->
          return ( buildSceneOutput scene (PrescriptFailed scriptException)
                 , scenarioGlobalVars
                 )

        Right (scenarioGlobalVarsAfterPrescript, scenarioLocalVarsAfterPrescript) -> do
          computeScene scene environmentVars scenarioGlobalVarsAfterPrescript scenarioLocalVarsAfterPrescript >>= \case
            SceneHttpComputation (Left error) ->
              return (buildSceneOutput scene (HttpSceneFailed error), scenarioGlobalVars)

            ScenePgComputation (Left error) ->
              return (buildSceneOutput scene (PgSceneFailed error), scenarioGlobalVars)

            SceneHttpComputation (Right httpComputation) ->
              let
                successfulSceneComputation = SuccesfulHttpSceneComputation httpComputation
              in
              case runPostscript scenarioGlobalVarsAfterPrescript Map.empty scene successfulSceneComputation of
                Left scriptException ->
                  return ( buildSceneOutput scene (HttpPostscriptFailed httpComputation scriptException)
                         , scenarioGlobalVarsAfterPrescript
                         )

                Right scenarioGlobalVarsAfterPostscript ->
                  return (buildSceneOutput scene (HttpSceneOk httpComputation), scenarioGlobalVarsAfterPostscript)

            ScenePgComputation (Right pgComputation) ->
              let
                successfulSceneComputation = SuccesfulPgSceneComputation pgComputation
              in
              case runPostscript scenarioGlobalVarsAfterPrescript Map.empty scene successfulSceneComputation of
                Left scriptException ->
                  return ( buildSceneOutput scene (PgPostscriptFailed pgComputation scriptException)
                         , scenarioGlobalVarsAfterPrescript
                         )

                Right scenarioGlobalVarsAfterPostscript ->
                  return (buildSceneOutput scene (PgSceneOk pgComputation), scenarioGlobalVarsAfterPostscript)


-- ** compute scene


computeScene
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => SceneFile
  -> EnvironmentVars
  -> ScenarioVars
  -> ScenarioVars
  -> m SceneComputationOutput
computeScene scene environmentVars scenarioGlobalVarsAfterPrescript scenarioLocalVarsAfterPrescript =
  case scene of
    HttpSceneFile { _sceneHttpInput } ->
      SceneHttpComputation <$> runRequestComputationWithScenarioContext _sceneHttpInput environmentVars scenarioGlobalVarsAfterPrescript scenarioLocalVarsAfterPrescript
    PgSceneFile { _scenePgInput } ->
      ScenePgComputation <$> runPgComputationWithScenarioContext _scenePgInput environmentVars scenarioGlobalVarsAfterPrescript scenarioLocalVarsAfterPrescript


-- ** pre script


runPrescript :: ScenarioVars -> ScenarioVars -> SceneFile -> Either ScriptException (ScenarioVars, ScenarioVars)
runPrescript scenarioGlobalVars scenarioLocalVars scene =
  foldl f (Right (scenarioGlobalVars, scenarioLocalVars)) (_scenePrescript scene)
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
  -> SceneFile
  -> SuccesfulSceneComputation
  -> Either ScriptException ScenarioVars
runPostscript scenarioGlobalVars scenarioLocalVars scene successfulSceneComputation =
  foldl f (Right (scenarioGlobalVars, scenarioLocalVars)) (_scenePostscript scene) <&> fst
  where
    f
      :: Either ScriptException (ScenarioVars, ScenarioVars)
      -> Proc
      -> Either ScriptException (ScenarioVars, ScenarioVars)
    f acc proc =
      acc >>= \env -> runPostscriptProc successfulSceneComputation env proc


-- ** run postscript proc


runPostscriptProc
  :: SuccesfulSceneComputation
  -> (ScenarioVars, ScenarioVars)
  -> Proc
  -> Either ScriptException (ScenarioVars, ScenarioVars)
runPostscriptProc successfulSceneComputation (scenarioGlobalVars, scenarioLocalVars) = \case
  AssertEqual expr1 expr2 ->
    let
      ex1 = runPostscriptExpr successfulSceneComputation scenarioGlobalVars scenarioLocalVars expr1
      ex2 = runPostscriptExpr successfulSceneComputation scenarioGlobalVars scenarioLocalVars expr2
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
    case runPostscriptExpr successfulSceneComputation scenarioGlobalVars scenarioLocalVars expr of
      Just newExpr ->
        let newLocalEnvironment = Map.insert var newExpr scenarioLocalVars
        in Right (scenarioGlobalVars, newLocalEnvironment)

      _ ->
        Left (UnknownVariable expr)

  Set var expr ->
    case runPostscriptExpr successfulSceneComputation scenarioGlobalVars scenarioLocalVars expr of
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
  :: SuccesfulSceneComputation
  -> ScenarioVars
  -> ScenarioVars
  -> Expr
  -> Maybe Expr
runPostscriptExpr succesfulSceneComputation scenarioGlobalVars scenarioLocalVars expr =
  case succesfulSceneComputation of
    SuccesfulHttpSceneComputation RequestComputation{..} ->
      case expr of
        Var var                  -> Map.lookup var scenarioLocalVars
        Fetch var                -> Map.lookup var scenarioGlobalVars
        HttpResponseBodyAsString -> Just (LString _requestComputationBody)
        HttpResponseStatus       -> Just (LInt _requestComputationStatusCode)
        expr                     -> Just expr

    SuccesfulPgSceneComputation _ ->
      case expr of
        Var var                  -> Map.lookup var scenarioLocalVars
        Fetch var                -> Map.lookup var scenarioGlobalVars
        HttpResponseBodyAsString -> Nothing
        HttpResponseStatus       -> Nothing
        expr                     -> Just expr


-- ** run prescript expr


runPrescriptExpr :: ScenarioVars -> ScenarioVars -> Expr -> Maybe Expr
runPrescriptExpr scenarioGlobalVars scenarioLocalVars = \case
  Var var -> Map.lookup var scenarioLocalVars
  Fetch var -> Map.lookup var scenarioGlobalVars
  HttpResponseBodyAsString -> Nothing
  expr -> Just expr


-- ** scene


buildSceneOutput :: SceneFile -> SceneComputation -> SceneOutput
buildSceneOutput scene sceneComputationOutput =
  SceneOutput { _outputSceneId = _sceneId scene
              , _outputSceneRequestFileNodeId = _sceneFileId scene
              , _outputSceneComputation = sceneComputationOutput
              }
