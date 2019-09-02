module BuilderApp.Message exposing (..)

import BuilderApp.Builder.Message as Builder
import EnvironmentToRunSelection.Message as EnvSelection

type Msg
  = DisplayBuilder Int
  | BuilderMsg Builder.Msg
