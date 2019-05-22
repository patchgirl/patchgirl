module BuilderApp.Message exposing (..)

import BuilderApp.Builder.Message as Builder

type Msg
  = SelectTab Int
  | BuilderMsg Builder.Msg
  | CloseTab Int
  | SaveTab Int
