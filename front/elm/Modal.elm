module Modal exposing (Modal(..), Config, view, map)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input


-- * model


type Modal
    = SelectHttpRequestModal
    | ConfirmScenarioFolderDeletionModal


-- * config


type alias Config msg =
    { closeMessage : Maybe msg
    , maskAttributes : List (Attribute msg)
    , containerAttributes : List (Attribute msg)
    , headerAttributes : List (Attribute msg)
    , bodyAttributes : List (Attribute msg)
    , footerAttributes : List (Attribute msg)
    , header : Maybe (Element msg)
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
            el
                ([ Background.color dialogMask
                 , width fill
                 , height fill
                 ]
                    ++ config.maskAttributes
                )
            <|
                column config.containerAttributes
                    [ wrapHeader config
                    , wrapBody config
                    , wrapFooter config
                    ]


wrapHeader : Config msg -> Element msg
wrapHeader { header, headerAttributes, closeMessage } =
    if header == Nothing && closeMessage == Nothing then
        none

    else
        row
            ([ width fill, padding 2 ] ++ headerAttributes)
            [ el [ alignLeft ] <| Maybe.withDefault none header
            , maybe none closeButton closeMessage
            ]


closeButton : msg -> Element msg
closeButton closeMessage =
    Input.button [ alignRight, padding 1, Font.size 16 ]
        { onPress = Just closeMessage
        , label = text "x"
        }


wrapBody : Config msg -> Element msg
wrapBody { body, bodyAttributes } =
    case body of
        Nothing ->
            none

        Just body_ ->
            el ([ width fill, padding 1 ] ++ bodyAttributes) body_


wrapFooter : Config msg -> Element msg
wrapFooter { footer, footerAttributes } =
    case footer of
        Nothing ->
            none

        Just footer_ ->
            el ([ width fill, padding 1 ] ++ footerAttributes) footer_


dialogMask =
    rgba 0 0 0 0.3



-- * util


map : (a -> b) -> Config a -> Config b
map f config =
    { closeMessage = Maybe.map f config.closeMessage
    , maskAttributes = List.map (Element.mapAttribute f) config.maskAttributes
    , containerAttributes = List.map (Element.mapAttribute f) config.containerAttributes
    , headerAttributes = List.map (Element.mapAttribute f) config.headerAttributes
    , bodyAttributes = List.map (Element.mapAttribute f) config.bodyAttributes
    , footerAttributes = List.map (Element.mapAttribute f) config.footerAttributes
    , header = Maybe.map (Element.map f) config.header
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
