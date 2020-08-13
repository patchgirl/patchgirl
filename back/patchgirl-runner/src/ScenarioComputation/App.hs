{-# LANGUAGE GADTs #-}

module ScenarioComputation.App ( runScenarioComputationHandler) where

import qualified Control.Monad             as Monad
import qualified Control.Monad.IO.Class    as IO
import qualified Control.Monad.Reader      as Reader
import qualified Control.Monad.State.Lazy  as State
import           Data.Function             ((&))
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
      :: (Reader.MonadReader Env m, IO.MonadIO m)
      => EnvironmentVars
      -> (ScenarioVars, [SceneOutput])
      -> SceneFile
      -> m (ScenarioVars, [SceneOutput])
    buildScenes environmentVars (scenarioGlobalVars, scenes) sceneFile = do
      case lastSceneWasSuccessful scenes of
        False -> return ( scenarioGlobalVars
                        , scenes ++ [ buildSceneOutput sceneFile SceneNotRun ]
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
  case State.runState (runScript (_scenePrescript scene) Nothing) (mkScriptContext PreScene scenarioGlobalVars) of
    (Just scriptException, scriptContextAfterPrescript) ->
      return ( buildSceneOutput scene (PrescriptFailed scriptException)
             , (globalVars scriptContextAfterPrescript)
             )

    (Nothing, scriptContextAfterPrescript) -> do
      runRequestComputationWithScenarioContext sceneHttpInput environmentVars (globalVars scriptContextAfterPrescript) (localVars scriptContextAfterPrescript) >>= \case
        Left error ->
          return (buildSceneOutput scene (HttpSceneFailed error), (globalVars scriptContextAfterPrescript))

        Right httpComputation ->
          case State.runState (runScript (_scenePostscript scene) Nothing) (mkScriptContext (PostScene httpComputation) (globalVars scriptContextAfterPrescript)) of
            (Just scriptException, _) ->
              return ( buildSceneOutput scene (HttpPostscriptFailed httpComputation scriptException)
                     , globalVars scriptContextAfterPrescript
                     )

            (Nothing, scriptContextAfterPostscript) ->
              return (buildSceneOutput scene (HttpSceneOk httpComputation)
                     , (globalVars scriptContextAfterPostscript)
                     )
  where
    mkScriptContext :: Context a -> ScenarioVars -> ScriptContext a
    mkScriptContext context globalVars =
      ScriptContext { globalVars = globalVars
                    , localVars = Map.empty
                    , context = context
                    }


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
  case State.runState (runScript (_scenePrescript scene) Nothing) (mkScriptContext PreScene scenarioGlobalVars) of
    (Just scriptException, scriptContextAfterPrescript) ->
      return ( buildSceneOutput scene (PrescriptFailed scriptException)
             , (globalVars scriptContextAfterPrescript)
             )

    (Nothing, scriptContextAfterPrescript) -> do
      runPgComputationWithScenarioContext scenePgInput environmentVars (globalVars scriptContextAfterPrescript) (localVars scriptContextAfterPrescript) >>= \case
        Left error ->
          return (buildSceneOutput scene (PgSceneFailed error), (globalVars scriptContextAfterPrescript))

        Right pgComputation ->
          case State.runState (runScript (_scenePostscript scene) Nothing) (mkScriptContext (PostScene pgComputation) (globalVars scriptContextAfterPrescript)) of
            (Just scriptException, _) ->
              return ( buildSceneOutput scene (PgPostscriptFailed pgComputation scriptException)
                     , globalVars scriptContextAfterPrescript
                     )

            (Nothing, scriptContextAfterPostscript) ->
              return (buildSceneOutput scene (PgSceneOk pgComputation)
                     , (globalVars scriptContextAfterPostscript)
                     )
  where
    mkScriptContext :: Context a -> ScenarioVars -> ScriptContext a
    mkScriptContext context globalVars =
      ScriptContext { globalVars = globalVars
                    , localVars = Map.empty
                    , context = context
                    }

-- * script

runScript
  :: TangoAst
  -> Maybe ScriptException
  -> State.State (ScriptContext a) (Maybe ScriptException)
runScript procs = \case
  exception@(Just _) -> return exception
  Nothing ->
    case procs of
      [] -> return Nothing
      proc : xs ->
        runProc proc >>= runScript xs


-- * proc


runProc :: Proc -> State.State (ScriptContext a) (Maybe ScriptException)
runProc = \case
  AssertEqual expr1 expr2 -> do
    ex1 <- reduceExprToPrimitive expr1
    ex2 <- reduceExprToPrimitive expr2
    case (ex1, ex2) of
      (Right a, Right b) ->
        case a == b of
          True  -> return $ Nothing
          False -> return $ Just (AssertEqualFailed a b)

      (Left s, _) ->
        return $ Just s

      (_, Left s) ->
        return $ Just s

  Let var expr -> do
    ex <- reduceExprToPrimitive expr
    case ex of
      Left s -> return $ Just s
      Right newExpr -> do
        scriptContext <- State.get
        let newLocalVars = Map.insert var newExpr (localVars scriptContext)
        State.put (scriptContext { localVars = newLocalVars })
        return Nothing

  Set var expr -> do
    ex <- reduceExprToPrimitive expr
    case ex of
      Left s -> return $ Just s
      Right newExpr -> do
        scriptContext <- State.get
        let newGlobalVars = Map.insert var newExpr (globalVars scriptContext)
        State.put (scriptContext { globalVars = newGlobalVars })
        return Nothing


-- * expr


fromTableToSimpleList :: [Row] -> Expr
fromTableToSimpleList rows =
  LList $ map rowToExpr rows
  where
    rowToExpr :: Row -> Expr
    rowToExpr (Row row) =
      LList $ map pgValueToExpr row

    pgValueToExpr :: (String, PgValue) -> Expr
    pgValueToExpr (_, pgValue) = case pgValue of
      PgString x -> LString x
      PgInt x    -> LInt x
      PgFloat x  -> LFloat x
      PgBool x   -> LBool x
      PgNull     -> LNull

fromTableToRichList :: [Row] -> Expr
fromTableToRichList rows =
  LList $ map rowToExpr rows
  where
    rowToExpr :: Row -> Expr
    rowToExpr (Row row) =
      LList $ map pgValueToExpr row

    pgValueToExpr :: (String, PgValue) -> Expr
    pgValueToExpr (name, pgValue) = case pgValue of
      PgString x -> LRowElem(name, LString x)
      PgInt x    -> LRowElem(name, LInt x)
      PgFloat x  -> LRowElem(name, LFloat x)
      PgBool x   -> LRowElem(name, LBool x)
      PgNull     -> LRowElem(name, LNull)


-- * context


data Context a where
  PreScene :: Context a
  PostScene :: ToPrimitive a => a -> Context a


-- * primitive


class ToPrimitive a where
  getBody :: a -> Either ScriptException Expr
  getStatus :: a -> Either ScriptException Expr
  getSimpleTable :: a -> Either ScriptException Expr
  getRichTable :: a -> Either ScriptException Expr


-- ** reduce to primite


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
  :: Expr
  -> State.State (ScriptContext a) (Either ScriptException Expr)
reduceExprToPrimitive = \case
  LHttpResponseStatus -> do
    context <- State.get <&> context
    case context of
      PreScene  -> return $ Left $ CannotUseFunction "You can't use httpResponseStatus in a prescript"
      PostScene result -> return $ getStatus result

  LHttpResponseBodyAsString -> do
    context <- State.get <&> context
    case context of
      PreScene  -> return $ Left $ CannotUseFunction "You can't use httpResponseBodyAsString in a prescript"
      PostScene result -> return $ getBody result

  LPgSimpleResponse -> do
    context <- State.get <&> context
    case context of
      PreScene  -> return $ Left $ CannotUseFunction "You can't use this function in a prescript"
      PostScene result -> return $ getSimpleTable result

  LPgRichResponse -> do
    context <- State.get <&> context
    case context of
      PreScene  -> return $ Left $ CannotUseFunction "You can't use this function in a prescript"
      PostScene result -> return $ getRichTable result

  LEq e1 e2 -> do
    b1 <- reduceExprToPrimitive e1
    b2 <- reduceExprToPrimitive e2
    return $ Right $ LBool (b1 == b2)

  lvar@(LVar var) -> do
    mValue <- State.get <&> localVars <&> Map.lookup var
    case mValue of
      Nothing   -> return $ Left $ UnknownVariable lvar
      Just expr -> return $ Right expr

  lfetch@(LFetch var) -> do
    mValue <- State.get <&> globalVars <&> Map.lookup var
    case mValue of
      Nothing   -> return $ Left $ UnknownVariable lfetch
      Just expr -> return $ Right expr

  LAccessOp ex1 ex2 -> do
    e1 <- reduceExprToPrimitive ex1
    e2 <- reduceExprToPrimitive ex2
    case (e1, e2) of
      (Right (LList list), Right (LInt index)) ->
        case getAtIndex list index of
          Just expr -> return $ Right expr
          Nothing   -> return $ Left AccessOutOfBound

      (Right (LList list), Right (LString index)) ->
        case getAtKey list index of
          error@(Left x) -> return $ Left x
          Right Nothing  -> return $ Right LNull
          Right (Just e) -> return $ Right e

      _ ->
        undefined

  e ->
    return $ Right e


-- ** request computation


instance ToPrimitive RequestComputation where
  getBody RequestComputation {..} =
    Right $ LString _requestComputationBody

  getStatus RequestComputation {..} =
    Right $ LInt _requestComputationStatusCode

  getSimpleTable _ =
    Left $ CannotUseFunction "function not available for http request"

  getRichTable _ =
    Left $ CannotUseFunction "function not available for http request"


-- ** pg computation


instance ToPrimitive PgComputation where
  getBody _ =
    Left $ CannotUseFunction "not available for pg query"

  getStatus _ =
    Left $ CannotUseFunction "not available for pg query"

  getSimpleTable = \case
    PgCommandOK ->
      Left $ EmptyResponse "empty response"

    PgTuplesOk table ->
      Right $ fromTableToSimpleList table

  getRichTable = \case
    PgCommandOK ->
      Left $ EmptyResponse "empty response"

    PgTuplesOk table ->
      Right $ fromTableToRichList table


-- * script context


data ScriptContext a =
  ScriptContext { globalVars :: ScenarioVars
                , localVars  :: ScenarioVars
                , context    :: Context a
                }


buildSceneOutput :: SceneFile -> SceneComputation -> SceneOutput
buildSceneOutput scene sceneComputationOutput =
  SceneOutput { _outputSceneId = _sceneId scene
              , _outputSceneRequestFileNodeId = _sceneFileId scene
              , _outputSceneComputation = sceneComputationOutput
              }

-- * util


getAtIndex :: [a] -> Int -> Maybe a
getAtIndex list index =
  case (list, index) of
    ([], _)      -> Nothing
    (x : xs, 0) -> Just x
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
            False -> Right $ Nothing

          e -> Left $ CantAccessElem e
