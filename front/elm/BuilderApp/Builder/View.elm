module BuilderApp.Builder.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import BuilderApp.Builder.Url
import BuilderApp.Builder.Header
import BuilderApp.Builder.Body
import BuilderApp.Builder.Response
import BuilderApp.Builder.Message exposing (..)
import BuilderApp.Builder.Model exposing (..)

view : Model -> Html Msg
view model =
  div [ id "builder" ]
    [ BuilderApp.Builder.Url.view model
    , BuilderApp.Builder.Header.view model
    , BuilderApp.Builder.Body.view
    , BuilderApp.Builder.Response.view model
    ]
