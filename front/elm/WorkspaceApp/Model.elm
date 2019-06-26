module WorkspaceApp.Model exposing (..)

import BuilderApp.Model as BuilderApp

type alias Model = List (String, BuilderApp.Model)

defaultModel : Model
defaultModel =
    [ ("MyWorkspace", BuilderApp.defaultModel)
    , ("Shared1", BuilderApp.emptyModel)
    ]
