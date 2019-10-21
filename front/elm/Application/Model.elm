module Application.Model exposing (..)

import InitializedApplication.Model as InitializedApplication

type Model
    = Unitialized
    | Initialized InitializedApplication.Model

defaultModel : Model
defaultModel = Unitialized
