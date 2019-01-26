module Util.View exposing (..)

import Json.Decode as Json
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

onEnter : a -> Attribute a
onEnter msg =
  let
    isEnter code =
      if code == 13 then
        Json.succeed msg
      else
        Json.fail "not ENTER"
    in
      on "keydown" (Json.andThen isEnter keyCode)
