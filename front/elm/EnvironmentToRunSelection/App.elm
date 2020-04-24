module EnvironmentToRunSelection.App exposing (..)

import Application.Type as Type
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import EnvironmentToRunSelection.Message exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Json
import List.Extra as List


type alias Model a =
    { a
        | selectedEnvironmentToRunIndex : Maybe Int
        , environments : List Type.Environment
    }


update : Msg -> Model a -> Model a
update msg model =
    case msg of
        Select idx ->
            case List.getAt idx (List.map .name model.environments) of
                Just _ ->
                    { model | selectedEnvironmentToRunIndex = Just idx }

                Nothing ->
                    { model | selectedEnvironmentToRunIndex = Nothing }


view : List String -> Element Msg
view environmentNames =
    html <|
        select [ on "change" (Json.map Select targetValueIntParse) ]
            (List.indexedMap entryView environmentNames)


entryView : Int -> String -> Html Msg
entryView idx envName =
    option [ Html.Attributes.value (String.fromInt idx) ] [ Html.text envName ]
