module Modal exposing (Modal(..), Config, view, map)

import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Util exposing(..)


-- * model


type Modal
    = SelectHttpRequestModal
    | ConfirmScenarioFolderDeletionModal


-- * config


type alias Config msg =
    { closeMessage : msg
    , header : Element msg
    , body : Maybe (Element msg)
    , footer : Maybe (Element msg)
    }


-- * view


view : Maybe (Config msg) -> Element msg
view maybeConfig =
    case maybeConfig of
        Nothing ->
            none

        Just config ->
            let
                modalView =
                    column [ centerX
                           , Background.color lightGrey
                           ]
                    [ headerView config
                    , bodyView config
                    , footerView config
                    ]
            in
                el [ Background.color (rgba 0 0 0 0.3)
                   , width fill
                   , height fill
                   ] <|
                    column [ width fill, height fill ]
                        [ el [ width fill, height (px 100), Events.onClick config.closeMessage ] none
                        , row [ width fill, height fill,paddingXY 0 100 ]
                            [ el [ Events.onClick config.closeMessage ] none
                            , modalView
                            , el [ Events.onClick config.closeMessage ] none
                            ]
                        , el [ width fill, height (px 100), Events.onClick config.closeMessage ] none
                        ]


-- ** header


headerView : Config msg -> Element msg
headerView { header, closeMessage } =
    row [ width fill, padding 2]
        [ el [ centerX ] header
        , closeButton closeMessage
        ]

closeButton : msg -> Element msg
closeButton closeMessage =
    Input.button [ alignRight, padding 1, Font.size 16 ]
        { onPress = Just closeMessage
        , label = text "x"
        }


-- ** body


bodyView : Config msg -> Element msg
bodyView { body } =
    case body of
        Nothing ->
            none

        Just body_ ->
            el [ width fill ] body_


-- ** footer


footerView : Config msg -> Element msg
footerView { footer } =
    case footer of
        Nothing ->
            none

        Just footer_ ->
            el [ centerX, width fill ] footer_


-- * util


map : (a -> b) -> Config a -> Config b
map f config =
    { closeMessage = f config.closeMessage
    , header = (Element.map f) config.header
    , body = Maybe.map (Element.map f) config.body
    , footer = Maybe.map (Element.map f) config.footer
    }

maybe : b -> (a -> b) -> Maybe a -> b
maybe default f value =
    case value of
        Just value_ ->
            f value_

        Nothing ->
            default
