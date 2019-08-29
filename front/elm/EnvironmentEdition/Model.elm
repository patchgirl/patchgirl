module EnvironmentEdition.Model exposing (..)

import Window.Type as Type

type alias Model a =
    { a
        | environments : List Type.Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , selectedEnvironmentToEditIndex : Maybe Int
        , selectedEnvironmentToRenameIndex : Maybe Int
    }
