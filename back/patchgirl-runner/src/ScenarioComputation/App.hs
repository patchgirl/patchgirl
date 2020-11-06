module ScenarioComputation.App ( runScenarioComputationHandler) where

import qualified Control.Monad             as Monad
import qualified Control.Monad.IO.Class    as IO
import qualified Control.Monad.Reader      as Reader
import qualified Control.Monad.State       as State
import           Data.Coerce               (coerce)
import           Data.Function             ((&))
import           Data.Functor              ((<&>))
import qualified Data.Map.Strict           as Map

import           Env
import           Interpolator
import           PgSqlComputation.App
import           PgSqlComputation.Model
import           RequestComputation.App
import           RequestComputation.Model
import           ScenarioComputation.Model
import           TangoScript.App
import           TangoScript.Model


-- * handler


runScenarioComputationHandler
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => ScenarioInput
  -> m ScenarioOutput
runScenarioComputationHandler ScenarioInput{..} = do
  let acc = ( emptyScenarioContext { _scenarioContextEnvironmentVars = _scenarioInputEnvVars }, [])
  Monad.foldM buildScenes acc _scenarioInputScenes <&> ScenarioOutput . snd
  where
    buildScenes
      :: ( Reader.MonadReader Env m
         , IO.MonadIO m
         )
      => (ScenarioContext, [SceneOutput])
      -> SceneFile
      -> m (ScenarioContext, [SceneOutput])
    buildScenes (scenarioContext, scenes) sceneFile = do
      case lastSceneWasSuccessful scenes of
        False -> return ( scenarioContext
                        , scenes ++ [ buildSceneOutput sceneFile SceneNotRun ]
                        )
        True -> do
          (scene, newScenarioContext) <- State.runStateT (buildScene sceneFile) scenarioContext
          return ( newScenarioContext
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
         , State.MonadState ScenarioContext m
         )
      => SceneFile
      -> m SceneOutput
    buildScene sceneFile = do
      State.modify $ \s -> s { _scenarioContextSceneVars = sceneFile & _sceneVariables
                             , _scenarioContextLocalVars = emptyScenarioVars
                             }
      case sceneFile of
        HttpSceneFile{..} -> runHttpScene sceneFile _sceneHttpInput
        PgSceneFile{..}   -> runPgScene sceneFile _scenePgInput


-- * scene


-- ** http


runHttpScene
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     , State.MonadState ScenarioContext m
     )
  => SceneFile
  -> TemplatedRequestComputationInput
  -> m SceneOutput
runHttpScene scene sceneHttpInput = do
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
     , State.MonadState ScenarioContext m
     )
  => SceneFile
  -> PgComputationInput
  -> m SceneOutput
runPgScene scene scenePgInput = do
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
  :: State.MonadState ScenarioContext m
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
  :: State.MonadState ScenarioContext m
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
          False -> return $ Just (AssertionFailed a b $ show a ++ " is not equal to " ++ show b)

      (Left s, _) ->
        return $ Just s

      (_, Left s) ->
        return $ Just s

  AssertNotEqual expr1 expr2 -> do
    ex1 <- reduceExprToPrimitive context expr1
    ex2 <- reduceExprToPrimitive context expr2
    case (ex1, ex2) of
      (Right a, Right b) ->
        case a /= b of
          True  -> return Nothing
          False -> return $ Just (AssertionFailed a b $ show a ++ " is equal to " ++ show b)

      (Left s, _) ->
        return $ Just s

      (_, Left s) ->
        return $ Just s

  Let var expr -> do
    ex <- reduceExprToPrimitive context expr
    case ex of
      Left s -> return $ Just s
      Right newExpr -> do
        oldLocalVars <- State.get <&> _scenarioContextLocalVars <&> coerce
        State.modify $ \s -> s { _scenarioContextLocalVars = ScenarioVars (Map.insert var newExpr oldLocalVars) }
        return Nothing

  Set var expr -> do
    ex <- reduceExprToPrimitive context expr
    case ex of
      Left s -> return $ Just s
      Right newExpr -> do
        oldGlobalVars <- State.get <&> _scenarioContextGlobalVars <&> coerce
        State.modify $ \s -> s { _scenarioContextGlobalVars = ScenarioVars (Map.insert var newExpr oldGlobalVars) }
        return Nothing
