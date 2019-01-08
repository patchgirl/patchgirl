module Builder.Body exposing (..)

import Html exposing (Html, Attribute, div, input, text, a, select, option, button, textarea, p)
import Html.Attributes exposing (value, placeholder, href, disabled)
import Html.Events exposing (onInput, onClick, keyCode, on)
import Html.Attributes exposing (hidden)

import Builder.Model exposing (Method(..), Model)
import Builder.Message exposing (Msg(..))

view : Model -> Html Msg
view model =
  let
    whenGetMethod = model.method == Get
  in
    div [ hidden whenGetMethod ]
      [ textarea [ placeholder "{ \n  \"your\": \"data\"\n}", onInput SetHttpBody ] []
      ]
