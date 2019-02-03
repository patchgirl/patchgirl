module EnvNav.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import EnvNav.Message exposing (..)
import EnvNav.Model exposing (..)

view : Model -> Html Msg
view model =
  ul [] <| List.map entryView model

entryView name =
  li [] [ text name ]
