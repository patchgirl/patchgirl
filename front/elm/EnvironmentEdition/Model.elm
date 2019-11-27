module EnvironmentEdition.Model exposing (..)

import Application.Type as Type

type alias Model a =
    { a
        | environments : List Type.Environment
        , selectedEnvironmentToEditId : Maybe Int
    }
