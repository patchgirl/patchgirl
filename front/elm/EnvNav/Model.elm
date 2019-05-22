module EnvNav.Model exposing (..)

import EnvApp.Model as EnvApp

type alias Model =
  { selectedEnvIndex : Maybe Int
  , renameEnvIdx : Maybe Int
  , envs : List(EnvInfo)
  }

type alias EnvInfo =
  { name : String
  , env : EnvApp.Model
  }

defaultEnvInfo =
  { name = "no name"
  , env = EnvApp.defaultModel
  }
