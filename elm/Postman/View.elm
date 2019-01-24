module Postman.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Postman.Model exposing (Model)
import Postman.Message exposing (Msg(..))

view : Model -> Html Msg
view model =
  div [ onClick Import ] [ text model ]
