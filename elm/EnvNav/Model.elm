module EnvNav.Model exposing (..)

import Env.Model as Env

type alias Model =
  { selectedEnvIndex : Maybe Int
  , displayedEnvIndexes : List(Int)
  , envs : List(Env)
  }

type alias Env =
  { name : String
  , env : Env.Model
  }
