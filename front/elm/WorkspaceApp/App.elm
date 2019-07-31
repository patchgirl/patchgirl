module WorkspaceApp.App exposing (..)

import List.Extra as List

import WorkspaceApp.Model exposing (..)
import WorkspaceApp.Message exposing (Msg(..))
import BuilderApp.Model as BuilderApp

update : Msg -> List Model -> List Model
update msg models =
    case msg of
        RenameWorkspace idx str ->
            let
                newModels = List.updateAt idx (\m -> { m | name = str }) models
            in
                newModels

        DeleteWorkspace idx ->
            let
                newModels = List.removeAt idx models
            in
                newModels

        AddNewInput ->
            let
                newModels = models ++ [ { name = "", builder = BuilderApp.defaultModel } ]
            in
                newModels
