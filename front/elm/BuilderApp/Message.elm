module BuilderApp.Message exposing (..)

import BuilderApp.Builder.Message as Builder
import EnvironmentToRunSelection.Message as EnvSelection
import BuilderApp.BuilderTree.Message as BuilderTree

type Msg
  = DisplayBuilder Int
  | BuilderMsg Builder.Msg
  | TreeMsg BuilderTree.Msg
  | ServerOk
  | ServerError String
