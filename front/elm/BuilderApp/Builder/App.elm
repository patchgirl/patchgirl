module BuilderApp.Builder.App exposing (..)

import Browser
import Html exposing (Html, Attribute, div, input, text, a, select, option, button, textarea, p)
import Html.Attributes exposing (value, placeholder, href, disabled, class, id)
import Html.Events exposing (onInput, onClick, keyCode, on)
import Http
import Debug
import Url
import Json.Decode as Json

import BuilderApp.Builder.Response
import BuilderApp.Builder.Message exposing (Msg(..))
import BuilderApp.Builder.Model exposing (Model, Method(..))
import BuilderApp.Builder.Header as Builder
import BuilderApp.Builder.Url
import BuilderApp.Builder.Body
import BuilderApp.Builder.Method as Builder

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateUrl url ->
      ( { model | url = url }, Cmd.none)

    SetHttpMethod newMethod ->
      case newMethod of
        "GET" -> ( { model | method = Get }, Cmd.none)
        _ -> ( { model | method = Post }, Cmd.none)

    SetHttpScheme scheme ->
      case scheme of
        "HTTP" -> ( { model | scheme = "HTTP" }, Cmd.none)
        _ -> ( { model | scheme = "HTTPS" }, Cmd.none)

    AskRun foo ->
      (model, Cmd.none)

    GiveResponse result ->
      ( { model | response = Just result }, Cmd.none)

    UpdateHeaders rawHeaders ->
      case Builder.parseHeaders rawHeaders of
        Just headers ->
          ( { model | headers = Debug.log "headers" headers }, Cmd.none )
        Nothing ->
          ( model, Cmd.none )

    SetHttpBody body ->
      ( { model | body = body }, Cmd.none )
