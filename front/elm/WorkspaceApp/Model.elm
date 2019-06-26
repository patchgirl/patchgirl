module WorkspaceApp.Model exposing (..)

import BuilderApp.Model as BuilderApp

type alias Model = List (String, BuilderApp.Model)

emptyModel : Model
emptyModel =
    [ ("MyWorkspace", BuilderApp.defaultModel)
    , ("Team1", BuilderApp.defaultModel)
    , ("Team2", BuilderApp.defaultModel)
    ]
