module HttpBody exposing (..)

import Html exposing (Html, Attribute, div, input, text, a, select, option, button, textarea, p)
import Html.Attributes exposing (value, placeholder, href, disabled)
import Html.Events exposing (onInput, onClick, keyCode, on)
import Html.Attributes exposing (hidden)

import Message exposing (Msg(..))
import HttpMethod exposing (Model(..))

type alias Model = String

view : HttpMethod.Model -> Html Msg
view method =
  let
    whenGetMethod = method == Get
  in
    div [ hidden whenGetMethod ]
      [ textarea [ placeholder "{ \n  \"your\": \"data\"\n}", onInput SetHttpBody ] []
      ]
