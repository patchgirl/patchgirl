module Application.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Application.Model exposing(..)
import Application.Message exposing(..)
import InitializedApplication.View as InitializedApplication

view : Model -> Html Msg
view model =
    case model of
        Unitialized ->
            div [] [ text "loading" ]
        Initialized initializedApplication ->
            div []
                [ Html.map InitializedApplicationMsg (InitializedApplication.view initializedApplication)
                ]
