module TangoScript.App where

import qualified Control.Monad             as Monad
import qualified Control.Monad.State       as State
import           Data.Functor              ((<&>))
import qualified Data.List                 as List
import qualified Data.Map.Strict           as Map

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
