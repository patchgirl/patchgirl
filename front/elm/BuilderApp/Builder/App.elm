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
import RequestRunner.Util as RequestRunner

update : Msg -> List(String, String) -> List(String, String) -> Model a -> (Model a, Cmd Msg)
update msg envKeyValues varKeyValues model =
    case msg of
        UpdateUrl newHttpUrl ->
            let
                newModel =
                    { model | httpUrl = changeEditedValue newHttpUrl model.httpUrl }
            in
                (newModel, Cmd.none)

        SetHttpMethod newMethod ->
            let
                newModel =
                    { model | httpMethod = newMethod }
            in
                (newModel, Cmd.none)

        UpdateHeaders rawHeaders ->
            let
                newModel =
                    case parseHeaders rawHeaders of
                        httpHeaders ->
                            { model | httpHeaders = changeEditedValue httpHeaders model.httpHeaders }
            in
                (newModel, Cmd.none)

        SetHttpBody httpBody ->
            let
                newModel =
                    { model | httpBody = changeEditedValue httpBody model.httpBody }
            in
                (newModel, Cmd.none)

        AskSave ->
            (model, Cmd.none)

        AskRun ->
            let
                newMsg =
                    buildRequestToRun envKeyValues varKeyValues model
                newModel =
                    { model
                        | showResponseView = True
                    }
            in
                (newModel, Debug.log "test" newMsg)

        ServerOk result ->
            let
                newModel =
                    { model | response = Just (convertResultToResponse result) }
            in
                (newModel, Cmd.none)

        ShowRequestAsCurl ->
            (model, Cmd.none)

        SetHttpBodyResponse newBody ->
            case model.response of
                Just response ->
                    let
                        newResponse = { response | body = newBody }
                        newModel = { model | response = Just newResponse }
                    in
                        (newModel, Cmd.none)

                _ ->
                    (model, Cmd.none)

parseHeaders : String -> List(String, String)
parseHeaders headers =
  let
    parseRawHeader : String -> (String, String)
    parseRawHeader rawHeader =
      case String.split ":" rawHeader of
        [headerKey, headerValue] -> (headerKey, headerValue)
        _ -> ("", "")
  in
      String.lines headers |> List.map parseRawHeader

buildRequestToRun : List(String, String) -> List(String, String) -> Model a -> Cmd Msg
buildRequestToRun envKeyValues varKeyValues builder =
    let
        request = RequestRunner.buildRequest <| RequestRunner.buildRequestInput envKeyValues varKeyValues builder
        cmdRequest =
            { method = request.method
            , headers = request.headers
            , url = request.url
            , body = request.body
            , expect = expectStringDetailed ServerOk
            , timeout = Nothing
            , tracker = Nothing
            }
    in
        Http.request cmdRequest


convertResponseStringToResult : Http.Response String -> Result ErrorDetailed ( Http.Metadata, String )
convertResponseStringToResult httpResponse =
    case httpResponse of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            Err (BadStatus metadata body)

        Http.GoodStatus_ metadata body ->
            Ok ( metadata, body )

convertResultToResponse : Result ErrorDetailed (Http.Metadata, String) -> Response
convertResultToResponse result =
    case result of
        Err (BadUrl url) ->
            Debug.todo "bad url"

        Err Timeout ->
            Debug.todo "timeout"

        Err NetworkError ->
            Debug.todo "network error"

        Err (BadStatus metadata body) ->
            { statusCode = metadata.statusCode
            , statusText = metadata.statusText
            , headers = metadata.headers
            , body = body
            }

        Ok (metadata, body) ->
            { statusCode = metadata.statusCode
            , statusText = metadata.statusText
            , headers = metadata.headers
            , body = body
            }


expectStringDetailed : (Result ErrorDetailed ( Http.Metadata, String ) -> msg) -> Http.Expect msg
expectStringDetailed msg =
    Http.expectStringResponse msg convertResponseStringToResult
