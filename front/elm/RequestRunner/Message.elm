module RequestRunner.Message exposing (..)

import Http

import Builder.Model as Builder
import Env.Model as Env
import VarApp.Model as VarApp

type Msg
  = Run Env.Model VarApp.Model Builder.Model
  | GetResponse (Result Http.Error String)
