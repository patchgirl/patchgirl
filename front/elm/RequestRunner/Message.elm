module RequestRunner.Message exposing (..)

import Http

import BuilderApp.Builder.Model as Builder
import VarApp.Model as VarApp

type Msg
  = Run (List (String, String)) VarApp.Model
  | GetResponse (Result Http.Error String)
