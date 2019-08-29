module RequestRunner.Message exposing (..)

import Http

import BuilderApp.Builder.Model as Builder
import EnvironmentEdition.Model as EnvironmentEdition
import VarApp.Model as VarApp

type Msg
  = Run EnvironmentEdition.Model VarApp.Model Builder.Model
  | GetResponse (Result Http.Error String)
