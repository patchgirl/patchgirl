module BuilderApp.Builder.App exposing (..)

import Browser
import Html exposing (Html, Attribute, div, input, text, a, select, option, button, textarea, p)
import Html.Attributes exposing (value, placeholder, href, disabled, class, id)
import Html.Events exposing (onInput, onClick, keyCode, on)
import Http
import Debug
import Url
import Json.Decode as Json

import Curl.Util as Curl

import BuilderApp.Builder.Response
import BuilderApp.Builder.Message exposing (..)
import BuilderApp.Builder.Model exposing (..)
import BuilderApp.Builder.Header as Builder
import BuilderApp.Builder.Url
import BuilderApp.Builder.Body
import BuilderApp.Builder.Method as Builder
import Api.Client as Client

update : Msg -> Model a -> Model a
update msg model =
    case msg of
        UpdateUrl url ->
            { model | url = url }

        SetHttpMethod newMethod ->
            case newMethod of
                "GET" ->
                    { model | method = Client.Get }
                _ ->
                    { model | method = Client.Post }

        GiveResponse result ->
            { model | response = Just result }

        UpdateHeaders rawHeaders ->
            case Builder.parseHeaders rawHeaders of
                Just headers ->
                    { model | headers = Debug.log "headers" headers }
                Nothing ->
                    model

        SetHttpBody body ->
            { model | body = body }

        AskSave ->
            model

        AskRun ->
            model

        ShowRequestAsCurl ->
            model
