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

import BuilderApp.Builder.Message exposing (..)
import BuilderApp.Builder.Model exposing (..)
import BuilderApp.Builder.Method as Builder
import Api.Client as Client
import Maybe.Extra as Maybe

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
            case parseHeaders rawHeaders of
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

parseHeaders : String -> Maybe (List(String, String))
parseHeaders headers =
  let
    parseRawHeader : String -> Maybe(String, String)
    parseRawHeader rawHeader =
      case String.split ":" rawHeader of
        [headerKey, headerValue] -> Just (headerKey, headerValue)
        _ -> Nothing
  in
    Maybe.traverse parseRawHeader (
      String.lines headers |> List.filter (not << String.isEmpty)
    )
