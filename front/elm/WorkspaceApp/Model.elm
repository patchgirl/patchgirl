module WorkspaceApp.Model exposing (..)

import BuilderApp.Model as BuilderApp

type alias Model = List
    { name : String
    , builder : BuilderApp.Model
    }

defaultModel : Model
defaultModel =
    [ { name = "MyWorkspace"
      , builder = BuilderApp.defaultModel
      }
    , { name = "Shared1"
      , builder = BuilderApp.emptyModel
      }
    ]
