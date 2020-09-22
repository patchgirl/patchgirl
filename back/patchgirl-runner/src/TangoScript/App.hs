module TangoScript.App where

import qualified Control.Monad             as Monad
import qualified Control.Monad.State       as State
import           Data.Functor              ((<&>))
import           Data.Function              ((&))
import qualified Data.List                 as List
import qualified Data.Map.Strict           as Map
import GHC.Natural (Natural)
import qualified GHC.Natural as Natural

import           ScenarioComputation.Model
import           TangoScript.Model


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
  json@(LJson _) ->
    return $ Right json

  LList exprs -> do
    Monad.mapM (reduceExprToPrimitive context) exprs <&> Monad.sequence >>= \case
      Right reducedExprs -> return $ Right $ LList reducedExprs
      Left s -> return $ Left s

  bool@(LBool _) ->
    return $ Right bool

  int@(LInt _) ->
    return $ Right int

  float@(LFloat _) ->
    return $ Right float

  LNull ->
    return $ Right LNull

  str@(LString _) ->
    return $ Right str

  rowElem@(LRowElem _) ->
    return $ Right rowElem

  lvar@(LVar var) -> do
    State.get <&> localVars <&> Map.lookup var >>= \case
      Nothing   -> return $ Left $ UnknownVariable lvar
      Just expr -> return $ Right expr

  lfetch@(LFetch var) -> do
    State.get <&> globalVars <&> Map.lookup var >>= \case
      Nothing   -> return $ Left $ UnknownVariable lfetch
      Just expr -> return $ Right expr

  LEq e1 e2 -> do
    b1 <- reduceExprToPrimitive context e1
    b2 <- reduceExprToPrimitive context e2
    return $ Right $ LBool (b1 == b2)

  LHttpResponseBodyAsString -> do
    case context of
      PreScene  -> return $ Left $ CannotUseFunction "You can't use httpResponseBodyAsString in a prescript"
      PostScene result -> return $ getBody result

  LHttpResponseStatus -> do
    case context of
      PreScene  -> return $ Left $ CannotUseFunction "You can't use httpResponseStatus in a prescript"
      PostScene result -> return $ getStatus result

  LPgSimpleResponse -> do
    case context of
      PreScene  -> return $ Left $ CannotUseFunction "You can't use this function in a prescript"
      PostScene result -> return $ getSimpleTable result

  LPgRichResponse -> do
    case context of
      PreScene  -> return $ Left $ CannotUseFunction "You can't use this function in a prescript"
      PostScene result -> return $ getRichTable result

  LAccessOp ex1 ex2 -> do
    e1 <- reduceExprToPrimitive context ex1
    e2 <- reduceExprToPrimitive context ex2
    case (e1, e2) of
      (Right expr1, Right expr2) -> return $ reduceAccessOp expr1 expr2
      (Left other, _) -> return $ Left other
      (_, Left other) -> return $ Left other


-- * reduce access op


reduceAccessOp :: Expr -> Expr -> Either ScriptException Expr
reduceAccessOp ex1 ex2 =
  case (ex1, ex2) of
    (LList list, LInt index) ->
      case getAtIndex list (fromIntegral index & Natural.naturalFromInteger) of
        Just expr -> Right expr
        Nothing   -> Left $ AccessOutOfBound ex1 ex2

    (LList list, LString index) ->
      case getAtKey list index of
        Right Nothing  -> Right LNull
        Right (Just e) -> Right e
        Left x         -> Left x

    (LString str, LInt index) ->
      getAtIndex str (fromIntegral index & Natural.naturalFromInteger) & \case
        Nothing -> Left $ AccessOutOfBound ex1 ex2
        Just letter -> Right (LString (letter : ""))

    (LJson (JString str), LInt index) ->
      getAtIndex str (fromIntegral index & Natural.naturalFromInteger) & \case
        Nothing -> Left $ CantAccessElem ex1 ex2
        Just letter -> Right (LString (letter : ""))

    (LJson (JArray list), LInt index) ->
      getAtIndex list (fromIntegral index & Natural.naturalFromInteger) & \case
        Just expr -> Right $ LJson expr
        Nothing -> Left $ AccessOutOfBound ex1 ex2

    (LJson (JObject object), LString str) ->
      Map.lookup str object & \case
        Just expr -> Right $ LJson expr
        Nothing -> Left $ AccessOutOfBound ex1 ex2

    (e, o) ->
      Left $ CantAccessElem e o

  where
    getAtIndex :: [a] -> Natural -> Maybe a
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
