module Modal exposing (Modal(..), Config, view, map)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
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
                           , moveDown 100
                           , padding 20
                           , spacing 30
                           , boxShadow
                           , Border.rounded 5
                           ]
                    [ headerView config
                    , bodyView config
                    , footerView config
                    ]
            in
                el [ Background.color (rgba 0 0 0 0.3)
                   , width fill
                   , height fill
                   , Events.onClick config.closeMessage
                   , inFront modalView
                   ] none


-- ** header


headerView : Config msg -> Element msg
headerView { header, closeMessage } =
    row [ width fill, spacing 20]
        [ el [] header
        , closeButton closeMessage
        ]

closeButton : msg -> Element msg
closeButton closeMessage =
    Input.button [ alignRight, Font.size 16 ]
        { onPress = Just closeMessage
        , label = clearIcon
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
