module EnvironmentToRunSelection.App exposing (..)

import EnvironmentToRunSelection.Message exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Json

import EnvironmentToRunSelection.Message exposing (..)
import EnvironmentToRunSelection.Model exposing (..)
import List.Extra as List
import Window.Type as Type

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

view : List String -> Html Msg
view environmentNames =
  select [ style "align-self" "flex-start", on "change" (Json.map Select targetValueIntParse) ]
    (List.indexedMap entryView environmentNames)

entryView : Int -> String -> Html Msg
entryView idx envName =
  option [ Html.Attributes.value (String.fromInt idx) ] [ text envName ]
