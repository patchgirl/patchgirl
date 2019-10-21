module BuilderApp.Builder.Body exposing (..)

import Html exposing (Html, Attribute, div, input, text, a, select, option, button, textarea, p)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import BuilderApp.Builder.Model exposing (..)
import BuilderApp.Builder.Message exposing (..)
import Debug

view : Html Msg
view =
  div [ id "bodyBuilder" ]
    [ textarea [ placeholder "{ \n  \"your\": \"data\"\n}", onInput SetHttpBody ] []
    ]
