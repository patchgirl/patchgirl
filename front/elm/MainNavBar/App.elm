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
import InitializedApplication.Model exposing (..)
import Application.Type exposing (..)

-- * model


type alias Model a =
    { a
      | session : Session
      , mainNavBarModel : MainNavBarModel
    }


-- * message


type Msg
    = OpenReqTab
    | OpenEnvTab
    | OpenLoginTab


-- * update


update : Msg -> Model a -> Model a
update msg model =
    case msg of
        OpenReqTab ->
            { model | mainNavBarModel = ReqTab }

        OpenEnvTab ->
            { model | mainNavBarModel = EnvTab }

        OpenLoginTab ->
            { model | mainNavBarModel = LoginTab }


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

rightView : Model a -> Element Msg
rightView model =
    case model.session of
        Visitor _ ->
            visitorRightView model

        SignedUser _ ->
            signedUserRightView model

visitorRightView : Model a -> Element Msg
visitorRightView model =
    row []
        [ link ([ paddingXY 20 0 ] ++ (mainLinkAttribute model OpenLoginTab LoginTab))
              { url = "#"
              , label = text "Login"
              }
        ]

signedUserRightView : Model a -> Element Msg
signedUserRightView model =
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

centerView : Model a -> Element Msg
centerView model =
    row [ centerX, paddingXY 10 0, centerY ]
        [ link (mainLinkAttribute model OpenReqTab ReqTab) { url = "#", label = text "Req" }
        , link (mainLinkAttribute model OpenEnvTab EnvTab) { url = "#", label = text "Env" }
        ]

mainLinkAttribute : Model a -> Msg -> MainNavBarModel -> List (Attribute Msg)
mainLinkAttribute model event mainNavBarModel =
            let
                activeAttribute =
                    [ Background.color <| secondaryColor
                    , Font.color <| primaryColor
                    ]

                passiveAttribute =
                    [ Font.color <| secondaryColor
                    ]

                activeOrPassiveAttribute =
                    case model.mainNavBarModel == mainNavBarModel of
                        True -> activeAttribute
                        False -> passiveAttribute

            in
                [ Events.onClick event
                , Font.size 21
                , paddingXY 15 19
                , mouseOver activeAttribute
                ] ++ activeOrPassiveAttribute


view : Model a -> Element Msg
view model =
    el [ width fill, Background.color primaryColor ] <|
        row [ width fill]--, explain Debug.todo]
            [ el [ alignLeft, paddingXY 20 0 ] leftView
            , el [ centerX ] <| centerView model
            , el [ alignRight ] (rightView model)
            ]
