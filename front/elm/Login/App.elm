module Login.App exposing (..)

import Application.Type exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Html exposing (Html)


-- * model


type alias Model a =
    { a | session : Session }


-- * message


type Msg
    = Nothing


-- * update


update : Msg -> Model a -> Model a
update msg model =
    model


-- * view


view : Model a -> Element Msg
view model =
    el [ width fill ] <|
        row [ width fill ]
            [ text "login"
            ]
