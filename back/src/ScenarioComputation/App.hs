{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

module ScenarioComputation.App ( runScenarioComputationHandler) where


import qualified Control.Monad             as Monad
import qualified Control.Monad.IO.Class    as IO
import qualified Control.Monad.Reader      as Reader
import           Data.Functor              ((<&>))
import qualified Data.Map.Strict           as Map

import           Environment.Model
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
runScenarioComputationHandler ScenarioInput{..} =
  Monad.foldM buildScenes (_scenarioInputGlobalEnv, []) _scenarioInputScenes <&> ScenarioOutput . snd
  where
    buildScenes
      :: (Reader.MonadReader Env m, IO.MonadIO m)
      => (ScenarioVars, [SceneOutput])
      -> SceneInput
      -> m (ScenarioVars, [SceneOutput])
    buildScenes (globalEnvironment, scenes) inputScene = do
      (scene, newGlobalEnvironment) <- runScene (lastSceneWasSuccessful scenes) globalEnvironment inputScene
      return (newGlobalEnvironment, scenes ++ [ scene ])

    lastSceneWasSuccessful :: [SceneOutput] -> Bool
    lastSceneWasSuccessful = \case
      [] -> True
      [x] -> case _outputSceneComputation x of
        SceneSucceeded _ -> True
        _                -> False
      (_:xs) -> lastSceneWasSuccessful xs


-- ** build scene


runScene :: (Reader.MonadReader Env m, IO.MonadIO m) => Bool -> ScenarioVars -> SceneInput -> m (SceneOutput, ScenarioVars)
runScene lastSceneWasSuccessful globalEnvironment sceneInput =
  case lastSceneWasSuccessful of
    True ->
      case runPrescript globalEnvironment (Map.fromList []) sceneInput of
        Left scriptException ->
          return ( buildSceneOutput sceneInput (PrescriptFailed scriptException)
                 , globalEnvironment
                 )

        Right globalEnvironmentAfterPrescript -> do
          requestComputationResult <- runRequestComputationHandler ( _sceneInputTemplatedRequestComputationInput sceneInput
                                                                   , Map.fromList [] -- globalEnvironmentAfterPrescript
                                                                   )
          case requestComputationResult of
            Left httpException ->
              return ( buildSceneOutput sceneInput (RequestFailed httpException)
                     , globalEnvironment
                     )

            Right requestComputationOutput ->
              case runPostscript globalEnvironmentAfterPrescript (Map.fromList []) sceneInput requestComputationOutput of
                Left scriptException ->
                  return ( buildSceneOutput sceneInput (PostscriptFailed scriptException)
                         , globalEnvironmentAfterPrescript
                         )

                Right globalEnvironmentAfterPostscript ->
                  return ( buildSceneOutput sceneInput (SceneSucceeded requestComputationOutput)
                         , globalEnvironmentAfterPostscript
                         )

    _ ->
      return ( buildSceneOutput sceneInput SceneNotRun
             , globalEnvironment
             )


-- ** pre script


runPrescript :: ScenarioVars -> ScenarioVars -> SceneInput -> Either ScriptException ScenarioVars
runPrescript globalEnvironment localEnvironment SceneInput{..} =
  foldl f (Right (globalEnvironment, localEnvironment)) _sceneInputPrescript <&> fst
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
runPostscript globalEnvironment localEnvironment SceneInput{..} requestComputationOutput =
  foldl f (Right (globalEnvironment, localEnvironment)) _sceneInputPostscript <&> fst
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
runPostscriptProc requestComputationOutput (globalEnvironment, localEnvironment) = \case
  AssertEqual expr1 expr2 ->
    let
      ex1 = runPostscriptExpr requestComputationOutput globalEnvironment localEnvironment expr1
      ex2 = runPostscriptExpr requestComputationOutput globalEnvironment localEnvironment expr2
    in
      case (ex1, ex2) of
        (Just a, Just b) ->
          case a == b of
            True  -> Right (globalEnvironment, localEnvironment)
            False -> Left (AssertEqualFailed a b)

        (Nothing, _) ->
          Left (UnknownVariable expr1)

        (_, Nothing) ->
          Left (UnknownVariable expr2)

  Let var expr ->
    case runPostscriptExpr requestComputationOutput globalEnvironment localEnvironment expr of
      Just newExpr ->
        let newLocalEnvironment = Map.insert var newExpr localEnvironment
        in Right (globalEnvironment, newLocalEnvironment)

      _ ->
        Left (UnknownVariable expr)

  Set var expr ->
    case runPostscriptExpr requestComputationOutput globalEnvironment localEnvironment expr of
      Just newExpr ->
        let newGlobalEnvironment = Map.insert var newExpr globalEnvironment
        in Right (newGlobalEnvironment, localEnvironment)

      _ ->
        Left (UnknownVariable expr)


-- ** run prescript proc


runPrescriptProc
  :: (ScenarioVars, ScenarioVars)
  -> Proc
  -> Either ScriptException (ScenarioVars, ScenarioVars)
runPrescriptProc (globalEnvironment, localEnvironment) = \case
  AssertEqual expr1 expr2 ->
    let
      ex1 = runPrescriptExpr globalEnvironment localEnvironment expr1
      ex2 = runPrescriptExpr globalEnvironment localEnvironment expr2
    in
      case (ex1, ex2) of
        (Just a, Just b) ->
          case a == b of
            True  -> Right (globalEnvironment, localEnvironment)
            False -> Left (AssertEqualFailed a b)

        (Nothing, _) ->
          Left (UnknownVariable expr1)

        (_, Nothing) ->
          Left (UnknownVariable expr2)

  Let var expr ->
    case runPrescriptExpr globalEnvironment localEnvironment expr of
      Just newExpr ->
        let newLocalEnvironment = Map.insert var newExpr localEnvironment
        in Right (globalEnvironment, newLocalEnvironment)

      _ ->
        Left (UnknownVariable expr)

  Set var expr ->
    case runPrescriptExpr globalEnvironment localEnvironment expr of
      Just newExpr ->
        let newGlobalEnvironment = Map.insert var newExpr globalEnvironment
        in Right (newGlobalEnvironment, localEnvironment)

      _ ->
        Left (UnknownVariable expr)


-- ** run postscript expr


runPostscriptExpr
  :: RequestComputationOutput
  -> ScenarioVars
  -> ScenarioVars
  -> Expr
  -> Maybe Expr
runPostscriptExpr RequestComputationOutput{..} globalEnvironment localEnvironment = \case
  Var var -> Map.lookup var localEnvironment
  Fetch var -> Map.lookup var globalEnvironment
  HttpResponseBodyAsString -> Just (LString _requestComputationOutputBody)
  expr -> Just expr


-- ** run prescript expr


runPrescriptExpr :: ScenarioVars -> ScenarioVars -> Expr -> Maybe Expr
runPrescriptExpr globalEnvironment localEnvironment = \case
  Var var -> Map.lookup var localEnvironment
  Fetch var -> Map.lookup var globalEnvironment
  HttpResponseBodyAsString -> Nothing
  expr -> Just expr


-- ** scene


buildSceneOutput :: SceneInput -> SceneComputation -> SceneOutput
buildSceneOutput SceneInput{..} sceneComputation =
  SceneOutput { _outputSceneId = _sceneInputId
              , _outputSceneRequestFileNodeId = _sceneInputRequestFileNodeId
              , _outputSceneComputation = sceneComputation
              }
