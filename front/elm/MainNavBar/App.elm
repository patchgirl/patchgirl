module MainNavBar.App exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font

import ViewUtil exposing (..)

import Html as Html
import Html.Attributes as Html
import Util.Route exposing (..)

-- * model

type Model
    = ReqTab
    | EnvTab

defaultModel = ReqTab

-- * message

type Msg
    = OpenReqTab
    | OpenEnvTab

-- * update

update : Msg -> Model -> Model
update msg model =
    case msg of
        OpenReqTab ->
            ReqTab
        OpenEnvTab ->
            EnvTab

-- * view

leftView : Element Msg
leftView =
    let
        linkContent =
            html <|
                Html.span [ Html.style "color" (colorToString secondaryColor)
                          , Html.style "font-size" "30px"
                          ]
                    [ Html.i
                          [ Html.class "material-icons"
                          , Html.style "font-size" "30px"
                          , Html.style "vertical-align" "bottom"
                          ]
                          [ Html.text "call_split" ]
                    , Html.text "ApiTester"
                    ]
    in
        link [] { url = href Home
                , label = linkContent
                }

rightView : Element Msg
rightView =
    let
        linkContent =
            html <|
                Html.span [ Html.style "color" (colorToString secondaryColor)
                          ]
                    [ Html.i
                          [ Html.class "material-icons"
                          , Html.style "vertical-align" "text-top"
                          ]
                          [ Html.text "menu" ]
                    , Html.text "Settings"
                    ]
    in
        link [ paddingXY 20 0 ]
            { url = href Settings
            , label = linkContent
            }

centerView : Model -> Element Msg
centerView model =
    let
        activeAttribute =
            [ Background.color <| secondaryColor
            , Font.color <| primaryColor
            ]
        passiveAttribute =
            [ Font.color <| secondaryColor
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
        row [ centerX, paddingXY 10 0, centerY ]
            [ link (attributes OpenReqTab ReqTab) { url = "#", label = text "Req" }
            , link (attributes OpenEnvTab EnvTab) { url = "#", label = text "Env" }
            ]


view : Model -> Element Msg
view model =
    el [ width fill, Background.color primaryColor ] <|
        row [ width fill]--, explain Debug.todo]
            [ el [ alignLeft, paddingXY 20 0 ] leftView
            , el [ centerX ] <| centerView model
            , el [ alignRight ] rightView
            ]
