module WorkspaceApp.App exposing (..)

import List.Extra as List

import WorkspaceApp.Model exposing (..)
import WorkspaceApp.Message exposing (Msg(..))
import BuilderApp.Model as BuilderApp

update : Msg -> Model -> Model
update msg model =
    case msg of
        RenameWorkspace idx str ->
            let
                newModel = List.updateAt idx (\(_, m) -> (str, m)) model
            in
                newModel

        DeleteWorkspace idx ->
            let
                newModel = List.removeAt idx model
            in
                newModel

        AddNewInput ->
            let
                newModel = model ++ [("", BuilderApp.defaultModel)]
            in
                newModel