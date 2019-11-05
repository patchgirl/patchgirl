module MainNavBar.View exposing (..)

import Element exposing (Element, rgb255, el, centerX, centerY, paddingXY, link, mouseOver, row, fill, width, Attribute, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font

import Bulma.Components as Bulma
import Bulma.Modifiers as Bulma

import MainNavBar.Model exposing(..)
import MainNavBar.Message exposing(..)

view : Model -> Element Msg
view model =
    let
        activeAttribute =
            [ Background.color <| rgb255 249 227 208
            , Font.color <| rgb255 220 118 118
            ]
        passiveAttribute =
            [ Font.color <| rgb255 249 227 208
            ]
        attributes : Msg -> Model -> List (Attribute Msg)
        attributes event model2 =
            let
                activeOrPassiveAttribute =
                    case model == model2 of
                        True -> activeAttribute
                        False -> passiveAttribute
            in
                [ Events.onClick event
                , Font.size 21
                , paddingXY 15 19
                , mouseOver activeAttribute
                ] ++ activeOrPassiveAttribute
    in
        el [ width fill, Background.color <| rgb255 220 118 118 ] <|
            row [ centerX, paddingXY 10 0, centerY ]
                [ link (attributes OpenReqTab ReqTab) { url = "#", label = text "Req" }
                , link (attributes OpenEnvTab EnvTab) { url = "#", label = text "Env" }
                , link (attributes OpenVarTab VarTab) { url = "#", label = text "Var" }
                ]
