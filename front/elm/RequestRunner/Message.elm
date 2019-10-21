module RequestRunner.Message exposing (..)

import Http

import BuilderApp.Builder.Model as Builder
import EnvironmentKeyValueEdition.Model as EnvironmentKeyValueEdition
import VarApp.Model as VarApp

type Msg
  = Run EnvironmentKeyValueEdition.Model VarApp.Model
  | GetResponse (Result Http.Error String)
