module Postman.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Postman.Message exposing (Msg(..))

view : Html Msg
view =
  div [ onClick Import ] [ text "import" ]
