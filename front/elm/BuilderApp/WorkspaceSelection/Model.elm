module BuilderApp.WorkspaceSelection.Model exposing (..)

type alias Model =
  { selectedWorkspaceIdx : Maybe Int
  , names : List String
  }
