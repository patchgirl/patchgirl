module EnvironmentToRunSelection.Model exposing (..)

type alias Model =
    { envs : List(String)
    , selectedEnvIdx : Maybe Int
    }
