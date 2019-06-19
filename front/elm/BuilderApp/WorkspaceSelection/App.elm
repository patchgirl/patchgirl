module BuilderApp.WorkspaceSelection.App exposing (..)

import BuilderApp.WorkspaceSelection.Model exposing (..)
import BuilderApp.WorkspaceSelection.Message exposing (Msg(..))

import List.Extra as List

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Select idx ->
      case List.getAt idx model.names of
        Just _ -> ( { model | selectedWorkspaceIdx = Just idx }, Cmd.none)
        Nothing -> ( { model | selectedWorkspaceIdx = Nothing }, Cmd.none)
