{-# LANGUAGE DeriveGeneric #-}

module TangoScript.Model ( TangoAst
                         , Proc(..)
                         , Expr(..)
                         , Json(..)
                         ) where

import qualified Data.Aeson      as Aeson
import           Data.Map.Strict (Map)
import           GHC.Generics    (Generic)


type TangoAst = [Proc]


-- * proc


data Proc
  = AssertEqual Expr Expr
  | AssertNotEqual Expr Expr
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
  | LHttpResponseBodyAsJson
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
    | JObject (Map String Json)
    | JNull
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON Json where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

instance Aeson.FromJSON Json where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }
