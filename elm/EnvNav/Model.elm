module EnvNav.Model exposing (..)

import Env.Model as Env

type alias Model =
  { selectedEnvIndex : Maybe Int
  , envs : List(EnvInfo)
  }

type alias EnvInfo =
  { name : String
  , env : Env.Model
  }
