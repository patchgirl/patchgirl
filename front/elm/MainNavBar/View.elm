module MainNavBar.View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Element as UI
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font

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
                    [ Background.color <| UI.rgb255 249 227 208
                    , Font.color <| UI.rgb255 220 118 118
                    ]
                passiveAttribute =
                    [ Font.color <| UI.rgb255 249 227 208
                    ]
                activeOrPassiveAttribute =
                    case model == model2 of
                        True -> activeAttribute
                        False -> passiveAttribute
            in
                [ Events.onClick event
                , Font.size 21
                , UI.paddingXY 15 19
                , UI.mouseOver activeAttribute
                ] ++ activeOrPassiveAttribute
    in
        UI.el [ UI.width UI.fill, Background.color <| UI.rgb255 220 118 118 ] <|
            UI.row [ UI.centerX, UI.paddingXY 10 0, UI.centerY ]
                [ UI.link (attributes OpenReqTab ReqTab) { url = "#", label = UI.text "Req" }
                , UI.link (attributes OpenEnvTab EnvTab) { url = "#", label = UI.text "Env" }
                , UI.link (attributes OpenVarTab VarTab) { url = "#", label = UI.text "Var" }
                ]
