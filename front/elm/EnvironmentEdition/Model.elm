module EnvironmentEdition.Model exposing (..)

import Application.Type as Type

type alias Model a =
    { a
        | environments : List Type.Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , selectedEnvironmentToEditId : Maybe Int
        , selectedEnvironmentToRenameId : Maybe Int
    }
