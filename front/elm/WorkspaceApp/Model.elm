module WorkspaceApp.Model exposing (..)

import BuilderApp.Model as BuilderApp

type alias Model =
    { name : String
    , builder : BuilderApp.Model
    }

defaultModel : List Model
defaultModel =
    [ { name = "MyWorkspace"
      , builder = BuilderApp.defaultModel
      }
    , { name = "Shared1"
      , builder = BuilderApp.emptyModel
      }
    ]
