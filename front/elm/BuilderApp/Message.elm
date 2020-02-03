module BuilderApp.Message exposing (..)

import BuilderApp.Builder.App as Builder
import EnvironmentToRunSelection.Message as EnvSelection
import BuilderApp.BuilderTree.Message as BuilderTree
import Http as Http

type Msg
  = BuilderMsg Builder.Msg
  | TreeMsg BuilderTree.Msg
  | EnvSelectionMsg Int
