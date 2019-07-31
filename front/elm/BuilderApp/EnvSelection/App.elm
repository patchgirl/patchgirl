module BuilderApp.EnvSelection.App exposing (..)

--import BuilderApp.EnvSelection.Model exposing (..)
import BuilderApp.EnvSelection.Message exposing (Msg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Json

import BuilderApp.EnvSelection.Message exposing (..)
import BuilderApp.EnvSelection.Model exposing (..)
import List.Extra as List

type alias Model a =
  { a
    | environmentNames : List(String)
    , selectedEnvironmentToRunIndex : Maybe Int
  }

update : Msg -> Model a -> Model a
update msg model =
    case msg of
        Select idx ->
            case List.getAt idx model.environmentNames of
                Just _ ->
                    { model | selectedEnvironmentToRunIndex = Just idx }
                Nothing ->
                    { model | selectedEnvironmentToRunIndex = Nothing }

view : Model a -> Html Msg
view model =
  select [ style "align-self" "flex-start", on "change" (Json.map Select targetValueIntParse) ]
    (List.indexedMap entryView model.environmentNames)

entryView : Int -> String -> Html Msg
entryView idx envName =
  option [ Html.Attributes.value (String.fromInt idx) ] [ text envName ]
