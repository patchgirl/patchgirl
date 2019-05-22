module RequestRunner.Message exposing (..)

import Http

import Builder.Model as Builder
import EnvApp.Model as EnvApp
import VarApp.Model as VarApp

type Msg
  = Run EnvApp.Model VarApp.Model Builder.Model
  | GetResponse (Result Http.Error String)
