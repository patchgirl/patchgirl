module Builder.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Builder.Url
import Builder.Header
import Builder.Body
import Builder.Response

import Builder.Message exposing (..)
import Builder.Model exposing (..)

view : Model -> Html Msg
view model =
  div [ id "builder" ]
    [ Builder.Url.view model
    , Builder.Header.view model
    , Builder.Body.view model
    , Builder.Response.view model
    ]
