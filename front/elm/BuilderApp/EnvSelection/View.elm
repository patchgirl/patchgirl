module BuilderApp.EnvSelection.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Json

import BuilderApp.EnvSelection.Message exposing (..)
import BuilderApp.EnvSelection.Model exposing (..)

view : Model -> Html Msg
view model =
  select [ style "align-self" "flex-start", on "change" (Json.map Select targetValueIntParse) ]
    (List.indexedMap entryView model.envs)

entryView : Int -> String -> Html Msg
entryView idx envName =
  option [ Html.Attributes.value (String.fromInt idx) ] [ text envName ]
