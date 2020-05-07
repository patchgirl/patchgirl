{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}

module ScenarioComputation.App ( runScenarioComputationHandler) where


import qualified Control.Monad             as Monad
import qualified Control.Monad.IO.Class    as IO
import qualified Control.Monad.Reader      as Reader
import           Data.Functor              ((<&>))
import qualified Data.Map.Strict           as Map
import           Debug.Trace

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
        Left scriptException ->
          return ( buildSceneOutput inputScene (PrescriptFailed scriptException)
                 , globalEnvironment
                 )

        Right globalEnvironmentAfterPrescript -> do
          requestComputationResult <- runRequestComputationHandler requestComputationInput
          case requestComputationResult of
            Left httpException ->
              return ( buildSceneOutput inputScene (RequestFailed httpException)
                     , globalEnvironment
                     )

            Right requestComputationOutput ->
              case runPostscript globalEnvironmentAfterPrescript (Map.fromList []) inputScene requestComputationOutput of
                Left scriptException ->
                  return ( buildSceneOutput inputScene (PostscriptFailed scriptException)
                         , globalEnvironmentAfterPrescript
                         )

                Right globalEnvironmentAfterPostscript ->
                  return ( buildSceneOutput inputScene (SceneSucceeded requestComputationOutput)
                         , globalEnvironmentAfterPostscript
                         )

    _ ->
      return ( buildSceneOutput inputScene SceneNotRun
             , globalEnvironment
             )


-- ** pre script


runPrescript :: ScenarioEnvironment -> ScenarioEnvironment -> SceneInput -> Either ScriptException ScenarioEnvironment
runPrescript globalEnvironment localEnvironment SceneInput{..} =
  foldl f (Right (globalEnvironment, localEnvironment)) _inputScenePrescript <&> fst
  where
    f
      :: Either ScriptException (ScenarioEnvironment, ScenarioEnvironment)
      -> Proc
      -> Either ScriptException (ScenarioEnvironment, ScenarioEnvironment)
    f acc proc =
      acc >>= \env -> runPrescriptProc env proc


-- ** run post script


runPostscript
  :: ScenarioEnvironment
  -> ScenarioEnvironment
  -> SceneInput
  -> RequestComputationOutput
  -> Either ScriptException ScenarioEnvironment
runPostscript globalEnvironment localEnvironment SceneInput{..} requestComputationOutput =
  foldl f (Right (globalEnvironment, localEnvironment)) _inputScenePostscript <&> fst
  where
    f
      :: Either ScriptException (ScenarioEnvironment, ScenarioEnvironment)
      -> Proc
      -> Either ScriptException (ScenarioEnvironment, ScenarioEnvironment)
    f acc proc =
      acc >>= \env -> runPostscriptProc requestComputationOutput env proc


-- ** run postscript proc


runPostscriptProc
  :: RequestComputationOutput
  -> (ScenarioEnvironment, ScenarioEnvironment)
  -> Proc
  -> Either ScriptException (ScenarioEnvironment, ScenarioEnvironment)
runPostscriptProc requestComputationOutput (globalEnvironment, localEnvironment) = \case
  AssertEqual expr1 expr2 ->
    let
      ex1 = trace "test" $ runPostscriptExpr requestComputationOutput globalEnvironment localEnvironment expr1
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
  :: (ScenarioEnvironment, ScenarioEnvironment)
  -> Proc
  -> Either ScriptException (ScenarioEnvironment, ScenarioEnvironment)
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
  -> ScenarioEnvironment
  -> ScenarioEnvironment
  -> Expr
  -> Maybe Expr
runPostscriptExpr RequestComputationOutput{..} globalEnvironment localEnvironment = \case
  Var var -> Map.lookup var localEnvironment
  Get var -> Map.lookup var globalEnvironment
  HttpResponseBodyAsString -> Just (LString _requestComputationOutputBody)
  expr -> Just expr


-- ** run prescript expr


runPrescriptExpr :: ScenarioEnvironment -> ScenarioEnvironment -> Expr -> Maybe Expr
runPrescriptExpr globalEnvironment localEnvironment = \case
  Var var -> Map.lookup var localEnvironment
  Get var -> Map.lookup var globalEnvironment
  HttpResponseBodyAsString -> Nothing
  expr -> Just expr


-- ** scene


buildSceneOutput :: SceneInput -> SceneComputation -> SceneOutput
buildSceneOutput SceneInput{..} sceneComputation =
  SceneOutput { _outputSceneId = _inputSceneId
              , _outputSceneRequestFileNodeId = _inputSceneRequestFileNodeId
              , _outputSceneComputation = sceneComputation
              }
