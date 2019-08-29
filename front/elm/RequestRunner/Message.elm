module RequestRunner.Message exposing (..)

import Http

import BuilderApp.Builder.Model as Builder
import EnvToRun.Model as EnvToRun
import VarApp.Model as VarApp

type Msg
  = Run EnvToRun.Model VarApp.Model Builder.Model
  | GetResponse (Result Http.Error String)
