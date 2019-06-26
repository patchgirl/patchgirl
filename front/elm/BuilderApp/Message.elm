module BuilderApp.Message exposing (..)

import BuilderApp.Builder.Message as Builder
import BuilderApp.EnvSelection.Message as EnvSelection

type Msg
  = SelectTab Int
  | BuilderMsg Builder.Msg
  | CloseTab Int
  | SaveTab Int
