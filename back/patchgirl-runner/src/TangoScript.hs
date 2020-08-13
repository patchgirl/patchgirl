{-# LANGUAGE DeriveGeneric #-}

module TangoScript ( TangoAst
                   , Proc(..)
                   , Expr(..)
                   , exprToString
                   ) where

import qualified Control.Monad       as Monad
import qualified Data.Aeson          as Aeson
import           Data.Function       ((&))
import           Data.Functor        ((<&>))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.List           as List
import           GHC.Generics        (Generic)


type TangoAst = [Proc]


-- * proc


data Proc
  = AssertEqual Expr Expr
  | Let String Expr
  | Set String Expr
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON Proc where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON Proc where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- * expr


data Expr
  = LJson Json
  | LList [Expr]
  | LBool Bool
  | LInt Int
  | LFloat Float
  | LNull
  | LString String
  | LRowElem (String, Expr)
  -- non primitive type
  | LVar String
  | LFetch String
  | LEq Expr Expr
  | LHttpResponseBodyAsString
  | LHttpResponseStatus
  | LPgSimpleResponse -- results without columnName
  | LPgRichResponse   -- response with columnName
  | LAccessOp Expr Expr
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON Expr where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON Expr where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }


-- ** json


data Json
    = JInt Int
    | JFloat Float
    | JBool Bool
    | JString String
    | JArray [Json]
    | JObject (HashMap String Json)
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON Json where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON Json where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

jsonToString :: Json -> String
jsonToString = \case
  JInt x -> show x
  JFloat x -> show x
  JBool x -> show x
  JString x -> show x
  JArray xs ->
    map jsonToString xs & List.intercalate ","
  JObject keyValues ->
    let
      showKeyValue :: [String] -> String -> Json -> [String]
      showKeyValue acc key value =
        acc ++ [ "\"" ++ key ++ "\":" ++ jsonToString value ]
    in
      Map.foldlWithKey' showKeyValue [] keyValues
        & List.intercalate ","
        & \str -> "{" ++ str ++ "}"


-- * util


exprToString :: Expr -> Maybe String
exprToString = \case
  LBool bool -> Just $ show bool
  LInt int -> Just $ show int
  LFloat float -> Just $ show float
  LNull -> Just "null"
  LString string -> Just string
  LList list ->
    Monad.mapM exprToString list <&> \l -> "[" ++ List.intercalate "," l ++ "]"
  LJson json -> Just $ jsonToString json
  LVar _ -> Nothing
  LFetch _ -> Nothing
  LEq _ _ -> Nothing
  LHttpResponseBodyAsString -> Nothing
  LHttpResponseStatus -> Nothing
  LPgSimpleResponse -> Nothing
  LPgRichResponse -> Nothing
  LAccessOp _ _ -> Nothing
