module ScenarioComputation.App ( runScenarioComputationHandler) where

import qualified Control.Monad             as Monad
import qualified Control.Monad.IO.Class    as IO
import qualified Control.Monad.Reader      as Reader
import qualified Control.Monad.State       as State
import           Data.Functor              ((<&>))
import qualified Data.List                 as List
import qualified Data.Map.Strict           as Map

import           Env
import           Interpolator
import           PgSqlComputation.App
import           PgSqlComputation.Model
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


-- * reduce to primitive


{-
  Expr is a recursive type.
  This function make sure an Expr is reduced to its limit so it cannot be recursive anymore
  eg:
    - LEq (LInt 1) (LInt 2) => LBool False
    - LAccessOp (LList [LInt 1, LInt 2]) (LInt 0) => LInt 1
    - LHttpResponseStatus => LInt theHttpStatus (if available)
    ...
-}
reduceExprToPrimitive
  :: State.MonadState ScriptContext m
  => Context a
  -> Expr
  -> m (Either ScriptException Expr)
reduceExprToPrimitive context = \case
  LList exprs -> do
    Monad.mapM (reduceExprToPrimitive context) exprs <&> Monad.sequence >>= \case
      Right reducedExprs -> return $ Right $ LList reducedExprs
      Left s -> return $ Left s

  LHttpResponseStatus -> do
    case context of
      PreScene  -> return $ Left $ CannotUseFunction "You can't use httpResponseStatus in a prescript"
      PostScene result -> return $ getStatus result

  LHttpResponseBodyAsString -> do
    case context of
      PreScene  -> return $ Left $ CannotUseFunction "You can't use httpResponseBodyAsString in a prescript"
      PostScene result -> return $ getBody result

  LPgSimpleResponse -> do
    case context of
      PreScene  -> return $ Left $ CannotUseFunction "You can't use this function in a prescript"
      PostScene result -> return $ getSimpleTable result

  LPgRichResponse -> do
    case context of
      PreScene  -> return $ Left $ CannotUseFunction "You can't use this function in a prescript"
      PostScene result -> return $ getRichTable result

  LEq e1 e2 -> do
    b1 <- reduceExprToPrimitive context e1
    b2 <- reduceExprToPrimitive context e2
    return $ Right $ LBool (b1 == b2)

  lvar@(LVar var) -> do
    State.get <&> localVars <&> Map.lookup var >>= \case
      Nothing   -> return $ Left $ UnknownVariable lvar
      Just expr -> return $ Right expr

  lfetch@(LFetch var) -> do
    State.get <&> globalVars <&> Map.lookup var >>= \case
      Nothing   -> return $ Left $ UnknownVariable lfetch
      Just expr -> return $ Right expr

  LAccessOp ex1 ex2 -> do
    e1 <- reduceExprToPrimitive context ex1
    e2 <- reduceExprToPrimitive context ex2
    case (e1, e2) of
      (Right (LList list), Right (LInt index)) ->
        case getAtIndex list index of
          Just expr -> return $ Right expr
          Nothing   -> return $ Left AccessOutOfBound

      (Right (LList list), Right (LString index)) ->
        case getAtKey list index of
          Right Nothing  -> return $ Right LNull
          Right (Just e) -> return $ Right e
          Left x         -> return $ Left x

      (Right e, Right o) ->
        return $ Left $ CantAccessElem e o

      (Left other, _) ->
        return $ Left other

      (_, Left other) ->
        return $ Left other

  e ->
    return $ Right e

  where
    getAtIndex :: [a] -> Int -> Maybe a
    getAtIndex list index =
      case (list, index) of
        ([], _)      -> Nothing
        (x : _, 0) -> Just x
        (_ : xs, n) ->
          case n < 0 of
            True  -> Nothing
            False -> getAtIndex xs (n - 1)

    getAtKey :: [Expr] -> String -> Either ScriptException (Maybe Expr)
    getAtKey list index =
      List.foldl' folder (Right Nothing) list
      where
        folder :: Either ScriptException (Maybe Expr) -> Expr -> Either ScriptException (Maybe Expr)
        folder acc expr =
          case acc of
            Left exception -> Left exception
            ok@(Right (Just _)) -> ok
            Right Nothing -> case expr of
              LRowElem (key, value) -> case key == index of
                True  -> Right $ Just value
                False -> Right Nothing

              e -> Left $ CantAccessElem e (LString index)
