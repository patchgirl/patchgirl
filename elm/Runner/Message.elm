module Runner.Message exposing (..)

import Http

import Builder.Model as Builder
import Env.Model as Env

type Msg
  = Run Env.Model Builder.Model
  | GetResponse (Result Http.Error String)
