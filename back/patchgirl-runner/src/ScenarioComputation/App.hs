module ScenarioComputation.App ( runScenarioComputationHandler) where

import qualified Control.Monad             as Monad
import qualified Control.Monad.IO.Class    as IO
import qualified Control.Monad.Reader      as Reader
import qualified Control.Monad.State.Lazy  as State
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
    buildScenes environmentVars (scenarioGlobalVars, scenes) sceneFile = do
      case lastSceneWasSuccessful scenes of
        False -> return ( scenarioGlobalVars
                        , scenes ++ [ (buildSceneOutput sceneFile SceneNotRun) ]
                        )
        True ->
          case sceneFile of
            HttpSceneFile{..} -> do
              (scene, newGlobalEnvironment) <- runHttpScene environmentVars scenarioGlobalVars sceneFile _sceneHttpInput
              return (newGlobalEnvironment, scenes ++ [ scene ])

            PgSceneFile{..} -> do
              (scene, newGlobalEnvironment) <- runPgScene environmentVars scenarioGlobalVars sceneFile _scenePgInput
              return (newGlobalEnvironment, scenes ++ [ scene ])

    lastSceneWasSuccessful :: [SceneOutput] -> Bool
    lastSceneWasSuccessful = \case
      [] -> True
      [x] -> case _outputSceneComputation x of
        HttpSceneOk _ -> True
        PgSceneOk _   -> True
        _             -> False
      (_:xs) -> lastSceneWasSuccessful xs


-- * scene


-- ** http


runHttpScene
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => EnvironmentVars
  -> ScenarioVars
  -> SceneFile
  -> TemplatedRequestComputationInput
  -> m (SceneOutput, ScenarioVars)
runHttpScene environmentVars scenarioGlobalVars scene sceneHttpInput =
  case State.runState (runHttpPrescript (_scenePrescript scene) Nothing) (scenarioGlobalVars, Map.empty) of
    (Just scriptException, (newGlobalVars, _)) ->
      return ( buildSceneOutput scene (PrescriptFailed scriptException)
             , newGlobalVars
             )

    (Nothing, (newGlobalVars, newLocalVars)) -> do
      runRequestComputationWithScenarioContext sceneHttpInput environmentVars newGlobalVars newLocalVars >>= \case
        Left error ->
          return (buildSceneOutput scene (HttpSceneFailed error), newGlobalVars)

        Right httpComputation ->
          case State.runState (runHttpPostscript (_scenePostscript scene) Nothing) (newGlobalVars, Map.empty, httpComputation) of
            (Just scriptException, _) ->
              return ( buildSceneOutput scene (HttpPostscriptFailed httpComputation scriptException)
                     , newGlobalVars
                     )

            (Nothing, (scenarioGlobalVarsAfterPostscript, _, _)) ->
              return (buildSceneOutput scene (HttpSceneOk httpComputation), scenarioGlobalVarsAfterPostscript)


-- ** pg


runPgScene
  :: ( Reader.MonadReader Env m
     , IO.MonadIO m
     )
  => EnvironmentVars
  -> ScenarioVars
  -> SceneFile
  -> PgComputationInput
  -> m (SceneOutput, ScenarioVars)
runPgScene environmentVars scenarioGlobalVars scene scenePgInput =
  case State.runState (runPgPrescript (_scenePrescript scene) Nothing) (scenarioGlobalVars, Map.empty) of
    (Just scriptException, (newGlobalVars, _)) ->
      return ( buildSceneOutput scene (PrescriptFailed scriptException)
             , newGlobalVars
             )

    (Nothing, (newGlobalVars, newLocalVars)) -> do
      runPgComputationWithScenarioContext scenePgInput environmentVars newGlobalVars newLocalVars >>= \case
        Left error ->
          return (buildSceneOutput scene (PgSceneFailed error), newGlobalVars)

        Right pgComputation ->
          case State.runState (runPgPostscript (_scenePostscript scene) Nothing) (newGlobalVars, Map.empty, pgComputation) of
            (Just scriptException, _) ->
              return ( buildSceneOutput scene (PgPostscriptFailed pgComputation scriptException)
                     , newGlobalVars
                     )

            (Nothing, (scenarioGlobalVarsAfterPostscript, _, _)) ->
              return (buildSceneOutput scene (PgSceneOk pgComputation), scenarioGlobalVarsAfterPostscript)


-- * prescript


runHttpPrescript :: TangoAst -> Maybe ScriptException -> State.State (ScenarioVars, ScenarioVars) (Maybe ScriptException)
runHttpPrescript procs = \case
  exception@(Just _) -> return exception
  Nothing ->
    case procs of
      [] -> return Nothing
      proc : xs ->
        runHttpPrescriptProc proc >>= runHttpPrescript xs

runPgPrescript :: TangoAst -> Maybe ScriptException -> State.State (ScenarioVars, ScenarioVars) (Maybe ScriptException)
runPgPrescript procs = \case
  exception@(Just _) -> return exception
  Nothing ->
    case procs of
      [] -> return Nothing
      proc : xs ->
        runPgPrescriptProc proc >>= runPgPrescript xs


-- ** proc


-- *** http


runHttpPrescriptProc
  :: Proc
  -> State.State (ScenarioVars, ScenarioVars) (Maybe ScriptException)
runHttpPrescriptProc = \case
  AssertEqual expr1 expr2 -> do
    ex1 <- runHttpPrescriptExpr expr1
    ex2 <- runHttpPrescriptExpr expr2
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
    ex <- runHttpPrescriptExpr expr
    case ex of
      Left s -> return $ Just s
      Right newExpr -> do
        (globalVars, localVars) <- State.get
        let newLocalVars = Map.insert var newExpr localVars
        State.put (globalVars, newLocalVars)
        return Nothing

  Set var expr -> do
    ex <- runHttpPrescriptExpr expr
    case ex of
      Left s -> return $ Just s
      Right newExpr -> do
        (globalVars, localVars) <- State.get
        let newGlobalVars = Map.insert var newExpr globalVars
        State.put (newGlobalVars, localVars)
        return Nothing


-- *** pg


runPgPrescriptProc
  :: Proc
  -> State.State (ScenarioVars, ScenarioVars) (Maybe ScriptException)
runPgPrescriptProc = \case
  AssertEqual expr1 expr2 -> do
    ex1 <- runPgPrescriptExpr expr1
    ex2 <- runPgPrescriptExpr expr2
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
    ex <- runPgPrescriptExpr expr
    case ex of
      Left s -> return $ Just s
      Right newExpr -> do
        (globalVars, localVars) <- State.get
        let newLocalVars = Map.insert var newExpr localVars
        State.put (globalVars, newLocalVars)
        return Nothing

  Set var expr -> do
    ex <- runPgPrescriptExpr expr
    case ex of
      Left s -> return $ Just s
      Right newExpr -> do
        (globalVars, localVars) <- State.get
        let newGlobalVars = Map.insert var newExpr globalVars
        State.put (newGlobalVars, localVars)
        return Nothing


-- ** expr


-- *** http


runHttpPrescriptExpr :: Expr -> State.State (ScenarioVars, ScenarioVars) (Either ScriptException Expr)
runHttpPrescriptExpr = \case
  var@(Var str) -> do
    localVars <- State.get <&> snd
    Map.lookup str localVars & \case
      Nothing -> return $ Left $ UnknownVariable var
      Just expr -> return $ Right expr

  var@(Fetch str) -> do
    globalVars <- State.get <&> fst
    Map.lookup str globalVars & \case
      Nothing -> return $ Left $ UnknownVariable var
      Just expr -> return $ Right expr

  HttpResponseBodyAsString ->
    return $ Left $ CannotUseFunction "You can't use `httpResponseBodyAsString` in a prescript"

  Eq e1 e2 ->
    return $ Right $ LBool (e1 == e2)

  Add _ _ ->
    return $ Left $ CannotUseFunction "not implemented yet"

  HttpResponseStatus ->
    return $ Left $ CannotUseFunction "You can't use `httpResponseStatus` in a prescript"

  PgResponseAsTable ->
    return $ Left $ CannotUseFunction "You can't use `pgResponseAsTable` in a prescript"

  rest ->
    return $ Right rest


-- *** pg


runPgPrescriptExpr :: Expr -> State.State (ScenarioVars, ScenarioVars) (Either ScriptException Expr)
runPgPrescriptExpr = runHttpPrescriptExpr


-- * postscript


runHttpPostscript :: TangoAst -> Maybe ScriptException -> State.State (ScenarioVars, ScenarioVars, RequestComputation) (Maybe ScriptException)
runHttpPostscript procs = \case
  exception@(Just _) -> return exception
  Nothing ->
    case procs of
      [] -> return Nothing
      proc : xs ->
        runHttpPostscriptProc proc >>= runHttpPostscript xs

runPgPostscript :: TangoAst -> Maybe ScriptException -> State.State (ScenarioVars, ScenarioVars, PgComputation) (Maybe ScriptException)
runPgPostscript procs = \case
  exception@(Just _) -> return exception
  Nothing ->
    case procs of
      [] -> return Nothing
      proc : xs ->
        runPgPostscriptProc proc >>= runPgPostscript xs


-- ** proc
-- *** http


runHttpPostscriptProc
  :: Proc
  -> State.State (ScenarioVars, ScenarioVars, RequestComputation) (Maybe ScriptException)
runHttpPostscriptProc = \case
  AssertEqual expr1 expr2 -> do
    ex1 <- runHttpPostscriptExpr expr1
    ex2 <- runHttpPostscriptExpr expr2
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
    ex <- runHttpPostscriptExpr expr
    case ex of
      Left s -> return $ Just s
      Right newExpr -> do
        (globalVars, localVars, reqComp) <- State.get
        let newLocalVars = Map.insert var newExpr localVars
        State.put (globalVars, newLocalVars, reqComp)
        return Nothing

  Set var expr -> do
    ex <- runHttpPostscriptExpr expr
    case ex of
      Left s -> return $ Just s
      Right newExpr -> do
        (globalVars, localVars, reqComp) <- State.get
        let newGlobalVars = Map.insert var newExpr globalVars
        State.put (newGlobalVars, localVars, reqComp)
        return Nothing

-- *** pg


runPgPostscriptProc
  :: Proc
  -> State.State (ScenarioVars, ScenarioVars, PgComputation) (Maybe ScriptException)
runPgPostscriptProc = \case
  AssertEqual expr1 expr2 -> do
    ex1 <- runPgPostscriptExpr expr1
    ex2 <- runPgPostscriptExpr expr2
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
    ex <- runPgPostscriptExpr expr
    case ex of
      Left s -> return $ Just s
      Right newExpr -> do
        (globalVars, localVars, pgComp) <- State.get
        let newLocalVars = Map.insert var newExpr localVars
        State.put (globalVars, newLocalVars, pgComp)
        return Nothing

  Set var expr -> do
    ex <- runPgPostscriptExpr expr
    case ex of
      Left s -> return $ Just s
      Right newExpr -> do
        (globalVars, localVars, pgComp) <- State.get
        let newGlobalVars = Map.insert var newExpr globalVars
        State.put (newGlobalVars, localVars, pgComp)
        return Nothing



-- ** expr


-- *** http


runHttpPostscriptExpr :: Expr -> State.State (ScenarioVars, ScenarioVars, RequestComputation) (Either ScriptException Expr)
runHttpPostscriptExpr = \case
  var@(Var str) -> do
    (_, localVars, _) <- State.get
    Map.lookup str localVars & \case
      Nothing -> return $ Left $ UnknownVariable var
      Just expr -> return $ Right expr

  var@(Fetch str) -> do
    (globalVars, _, _) <- State.get
    Map.lookup str globalVars & \case
      Nothing -> return $ Left $ UnknownVariable var
      Just expr -> return $ Right expr

  Eq e1 e2 ->
    return $ Right $ LBool (e1 == e2)

  Add _ _ ->
    return $ Left $ CannotUseFunction "not implemented yet"

  HttpResponseBodyAsString -> do
    (_, _, RequestComputation {..})<- State.get
    return $ Right $ LString _requestComputationBody

  HttpResponseStatus -> do
    (_, _, RequestComputation {..})<- State.get
    return $ Right $ LInt _requestComputationStatusCode

  PgResponseAsTable ->
    return $ Left $ CannotUseFunction "You can't use `pgResponseAsTable` in an http postscript "

  rest ->
    return $ Right rest


-- *** pg


runPgPostscriptExpr :: Expr -> State.State (ScenarioVars, ScenarioVars, PgComputation) (Either ScriptException Expr)
runPgPostscriptExpr = \case
  var@(Var str) -> do
    (_, localVars, _) <- State.get
    Map.lookup str localVars & \case
      Nothing -> return $ Left $ UnknownVariable var
      Just expr -> return $ Right expr

  var@(Fetch str) -> do
    (globalVars, _, _) <- State.get
    Map.lookup str globalVars & \case
      Nothing -> return $ Left $ UnknownVariable var
      Just expr -> return $ Right expr

  Eq e1 e2 ->
    return $ Right $ LBool (e1 == e2)

  Add _ _ ->
    return $ Left $ CannotUseFunction "not implemented yet"

  HttpResponseBodyAsString ->
    return $ Left $ CannotUseFunction "You can't use `httpResponseBodyAsString` in an pg postscript "

  HttpResponseStatus ->
    return $ Left $ CannotUseFunction "You can't use `httpResponseStatus` in an pg postscript "

  PgResponseAsTable -> do
    (_, _, pgComputation) <- State.get
    case pgComputation of
      PgCommandOK ->
        return $ Left $ EmptyResponse "postgresql query didn't return any data"
      PgTuplesOk table ->
        return $ Right $ fromTableToList table

  rest ->
    return $ Right rest


-- * util


buildSceneOutput :: SceneFile -> SceneComputation -> SceneOutput
buildSceneOutput scene sceneComputationOutput =
  SceneOutput { _outputSceneId = _sceneId scene
              , _outputSceneRequestFileNodeId = _sceneFileId scene
              , _outputSceneComputation = sceneComputationOutput
              }

fromTableToList :: Table -> Expr
fromTableToList (Table columns) =
  EList $ map columnToList columns
  where
    columnToList :: Column -> Expr
    columnToList (Column _ pgValues) =
      EList $ map pgValueToExpr pgValues

    pgValueToExpr :: PgValue -> Expr
    pgValueToExpr = \case
      PgString x -> LString x
      PgInt x -> LInt x
      PgFloat x -> LFloat x
      PgBool x -> LBool x
      PgNull -> LNull
