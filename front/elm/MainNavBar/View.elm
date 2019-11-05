module MainNavBar.View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Element as UI
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events

import Bulma.Components as Bulma
import Bulma.Modifiers as Bulma

import MainNavBar.Model exposing(..)
import MainNavBar.Message exposing(..)

view : Model -> UI.Element Msg
view model =
    let
        attributes : Msg -> Model -> List (UI.Attribute Msg)
        attributes event model2 =
            let
                activeAttribute =
                    case model == model2 of
                        True ->
                            [ Background.color (UI.rgb 0 0.5 0) ]
                        False ->
                            []
            in
                [ Events.onClick event
                , Border.color (UI.rgb 0 0.7 0)
                ] ++ activeAttribute
    in
        UI.el [ UI.centerX ] <|
            UI.row [ UI.spacing 30 ]
                [ UI.el (attributes OpenReqTab ReqTab) (UI.link [] { url = "#", label = UI.text "Req" })
                , UI.el (attributes OpenEnvTab EnvTab) (UI.link [] { url = "#", label = UI.text "Env" })
                , UI.el (attributes OpenVarTab VarTab) (UI.link [] { url = "#", label = UI.text "Var" })
                ]
