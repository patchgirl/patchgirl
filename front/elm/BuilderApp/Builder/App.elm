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
import Application.Type exposing (..)

update : Msg -> Model a -> Model a
update msg model =
    case msg of
        UpdateUrl newHttpUrl ->
            { model | httpUrl = changeEditedValue newHttpUrl model.httpUrl }

        SetHttpMethod newMethod ->
            { model | httpMethod = newMethod }

        GiveResponse result ->
            { model | response = Just result }

        UpdateHeaders rawHeaders ->
            case parseHeaders rawHeaders of
                Just httpHeaders ->
                    { model | httpHeaders = Debug.log "headers" httpHeaders }
                Nothing ->
                    model

        SetHttpBody httpBody ->
            { model | httpBody = httpBody }

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
