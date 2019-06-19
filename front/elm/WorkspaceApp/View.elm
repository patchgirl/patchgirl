module WorkspaceApp.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import WorkspaceApp.Message exposing (..)
import WorkspaceApp.Model exposing (..)

view : Model -> Html Msg
view model =
    div [ id "varApp", class "columns" ]
        [ ul [ class "column" ] [] ]
