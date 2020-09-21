module ScenarioComputation.App ( runScenarioComputationHandler) where

import qualified Control.Monad             as Monad
import qualified Control.Monad.IO.Class    as IO
import qualified Control.Monad.Reader      as Reader
import qualified Control.Monad.State       as State
import           Data.Functor              ((<&>))
import qualified Data.Map.Strict           as Map

import           Env
import           Interpolator
import           PgSqlComputation.App
import           PgSqlComputation.Model
import           RequestComputation.App
import           RequestComputation.Model
import           ScenarioComputation.Model
import           TangoScript.Model
import           TangoScript.App


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
      :: ( Reader.MonadReader Env m
         , IO.MonadIO m
         )
      => EnvironmentVars
      -> (ScenarioVars, [SceneOutput])
      -> SceneFile
      -> m (ScenarioVars, [SceneOutput])
    buildScenes environmentVars (scenarioGlobalVars, scenes) sceneFile = do
      case lastSceneWasSuccessful scenes of
        False -> return ( scenarioGlobalVars
                        , scenes ++ [ buildSceneOutput sceneFile SceneNotRun ]
                        )
        True -> do
          (scene, scriptContext) <- State.runStateT (buildScene sceneFile) (ScriptContext environmentVars scenarioGlobalVars Map.empty)
          return ( globalVars scriptContext
                 , scenes ++ [ scene ]
                 )

    lastSceneWasSuccessful :: [SceneOutput] -> Bool
    lastSceneWasSuccessful = \case
      [] -> True
      [x] -> case _outputSceneComputation x of
        HttpSceneOk _ -> True
        PgSceneOk _   -> True
        _             -> False
      (_:xs) -> lastSceneWasSuccessful xs

    buildScene
      :: ( Reader.MonadReader Env m
         , IO.MonadIO m
         , State.MonadState ScriptContext m
         )
      => SceneFile
      -> m SceneOutput
    buildScene sceneFile =
      case sceneFile of
        HttpSceneFile{..} -> runHttpScene sceneFile _sceneHttpInput
        PgSceneFile{..}   -> runPgScene sceneFile _scenePgInput


-- * scene


-- ** http


runHttpScene
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , State.MonadState ScriptContext m
     )
  => SceneFile
  -> TemplatedRequestComputationInput
  -> m SceneOutput
runHttpScene scene sceneHttpInput = do
  State.modify $ \s -> s { localVars = Map.empty }
  runScript (_scenePrescript scene) PreScene Nothing >>= \case
    Just scriptException ->
      return $ buildSceneOutput scene (PrescriptFailed scriptException)

    Nothing -> do
      runRequestComputationWithScenarioContext sceneHttpInput >>= \case
        Left error ->
          return $ buildSceneOutput scene (HttpSceneFailed error)

        Right httpComputation -> do
          runScript (_scenePostscript scene) (PostScene httpComputation) Nothing >>= \case
            Just scriptException ->
              return $ buildSceneOutput scene (HttpPostscriptFailed httpComputation scriptException)

            Nothing ->
              return $ buildSceneOutput scene (HttpSceneOk httpComputation)


-- ** pg


runPgScene
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , State.MonadState ScriptContext m
     )
  => SceneFile
  -> PgComputationInput
  -> m SceneOutput
runPgScene scene scenePgInput = do
  State.modify $ \s -> s { localVars = Map.empty }
  runScript (_scenePrescript scene) PreScene Nothing >>= \case
    Just scriptException ->
      return $ buildSceneOutput scene (PrescriptFailed scriptException)

    Nothing -> do
      runPgComputationWithScenarioContext scenePgInput >>= \case
        Left error ->
          return $ buildSceneOutput scene (PgSceneFailed error)

        Right pgComputation -> do
          runScript (_scenePostscript scene) (PostScene pgComputation) Nothing >>= \case
            Just scriptException ->
              return $ buildSceneOutput scene (PgPostscriptFailed pgComputation scriptException)

            Nothing ->
              return $ buildSceneOutput scene (PgSceneOk pgComputation)


-- * script


runScript
  :: State.MonadState ScriptContext m
  => TangoAst
  -> Context a
  -> Maybe ScriptException
  -> m (Maybe ScriptException)
runScript procs context = \case
  Nothing ->
    case procs of
      [] -> return Nothing
      proc : xs ->
        runProc context proc >>= runScript xs context
  exception -> return exception


-- * proc


runProc
  :: State.MonadState ScriptContext m
  => Context a
  -> Proc
  -> m (Maybe ScriptException)
runProc context = \case
  AssertEqual expr1 expr2 -> do
    ex1 <- reduceExprToPrimitive context expr1
    ex2 <- reduceExprToPrimitive context expr2
    case (ex1, ex2) of
      (Right a, Right b) ->
        case a == b of
          True  -> return Nothing
          False -> return $ Just (AssertEqualFailed a b)

      (Left s, _) ->
        return $ Just s

      (_, Left s) ->
        return $ Just s

  Let var expr -> do
    ex <- reduceExprToPrimitive context expr
    case ex of
      Left s -> return $ Just s
      Right newExpr -> do
        State.modify $ \state -> state { localVars = Map.insert var newExpr (localVars state) }
        return Nothing

  Set var expr -> do
    ex <- reduceExprToPrimitive context expr
    case ex of
      Left s -> return $ Just s
      Right newExpr -> do
        State.modify $ \state -> state { globalVars = Map.insert var newExpr (globalVars state) }
        return Nothing
