module BuilderApp.Builder.App exposing (..)

import Browser
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


import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Events as Events
import Element.Input as Input

import ViewUtil exposing (..)

import Html as Html
import Html.Attributes as Html
import Html.Events as Html

import BuilderApp.Builder.Message exposing (..)
import BuilderApp.Builder.Model exposing (..)
import Util.View as Util
import Application.Type exposing (..)
import Dict as Dict
import Json.Print as Json


-- * update


update : Msg -> List (Storable NewKeyValue KeyValue) -> List KeyValue -> Model a -> (Model a, Cmd Msg)
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
                    { model | httpMethod = changeEditedValue newMethod model.httpMethod }
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

buildRequestToRun : List (Storable NewKeyValue KeyValue) -> List KeyValue -> Model a -> Cmd Msg
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


-- * view


view : Model a -> Element Msg
view model =
    let
        builderView =
            column [ width fill, spacing 10 ]
                [ column [ width fill ]
                      [ row [ width fill, spacing 10 ]
                            [ urlView model
                            ]
                      ]
                , methodView model
                , headerView model
                , bodyView model
                ]
    in
        case model.showResponseView of
            False ->
                column [ width fill ]
                    [ titleView model
                    , el [ width fill ] builderView
                    ]

            True ->
                column [ width fill ]
                    [ titleView model
                    , row [ width fill, spacing 20 ]
                        [ el [ width (fillPortion 1), alignTop ] builderView
                        , el [ width (fillPortion 1), alignTop ] (responseView model)
                        ]
                    ]

titleView : Model a -> Element Msg
titleView model =
    let
        name = notEditedValue model.name
    in
        row [ centerX, paddingXY 0 10, spacing 10 ]
            [ el [] <| iconWithTextAndColor "label" (name) secondaryColor
            , mainActionButtonsView model
            ]

responseView : Model a -> Element Msg
responseView model =
    let
        bodyResponseText : String -> Dict.Dict String String -> String
        bodyResponseText body responseHeaders =
            case Dict.get "content-type" responseHeaders of
                Just contentType ->
                    case String.contains "application/json" contentType of
                        True -> Result.withDefault body (Json.prettyString { indent = 4, columns = 4 } body)
                        False -> body
                _ -> body

        statusResponseView : Response -> Element Msg
        statusResponseView response =
            let
                statusText =
                    String.fromInt response.statusCode ++ " - " ++ response.statusText
                statusLabel =
                    if response.statusCode >= 200 && response.statusCode < 300 then
                        labelSuccess statusText
                    else if response.statusCode >= 400 && response.statusCode < 500 then
                        labelWarning statusText
                    else if response.statusCode >= 500 then
                        labelError statusText
                    else
                        labelWarning statusText
                    in
                        column [ spacing 5 ]
                            [ text "status: "
                            , statusLabel
                            ]


        headersResponseView : Response -> Element Msg
        headersResponseView response =
            let
                headers =
                    Dict.toList response.headers
                        |> List.map (joinTuple ": ")
                        |> String.join "\n"
            in
                Input.multiline []
                    { onChange = SetHttpBody
                    , text = headers
                    , placeholder = Nothing
                    , label = labelInputView "Headers: "
                    , spellcheck = False
                    }

        bodyResponseView : Response -> Element Msg
        bodyResponseView response =
            case response.body of
                "" ->
                    none

                _ ->
                    Input.multiline []
                        { onChange = SetHttpBodyResponse
                        , text = bodyResponseText response.body response.headers
                        , placeholder = Nothing                                , label = labelInputView "body: "
                        , spellcheck = False
                        }
    in
        case model.response of
            Nothing ->
                none

            Just response ->
                column [ spacing 10 ]
                    [ statusResponseView response
                    , bodyResponseView response
                    , headersResponseView response
                    ]


urlView : Model a -> Element Msg
urlView model =
    el [ alignLeft, width fill ] <|
        Input.text [ htmlAttribute <| Util.onEnter AskRun ]
            { onChange = UpdateUrl
            , text = editedOrNotEditedValue model.httpUrl
            , placeholder = Just <| Input.placeholder [] (text "myApi.com/path?arg=someArg")
            , label = labelInputView "Url: "
            }

mainActionButtonsView : Model a -> Element Msg
mainActionButtonsView model =
    let
        rowParam =
            [ centerY
            , height fill
            , spacing 10
            , alignRight
            , Font.color primaryColor
            ]

        inputParam =
            [ Border.solid
            , Border.color secondaryColor
            , Border.width 1
            , Border.rounded 5
            , alignBottom
            , Background.color secondaryColor
            , paddingXY 10 10
            ]

    in
        row rowParam
            [ Input.button inputParam
                { onPress = Just <| AskRun
                , label = el [ centerY] <| iconWithTextAndColor "send" "Send" primaryColor
                }
            , case isBuilderDirty model of
                  True ->
                      Input.button inputParam
                          { onPress = Just <| AskSave
                          , label = el [ centerY] <| iconWithTextAndColor "save" "Save" primaryColor
                          }

                  False ->
                      none
            ]

methodView : Model a -> Element Msg
methodView model =
    Input.radioRow [ padding 10, spacing 20 ]
        { onChange = SetHttpMethod
        , selected = Just <| editedOrNotEditedValue model.httpMethod
        , label = labelInputView "Method: "
        , options =
              [ Input.option Client.Get (text "Get")
              , Input.option Client.Post (text "Post")
              , Input.option Client.Put (text "Put")
              , Input.option Client.Delete (text "Delete")
              , Input.option Client.Patch (text "Patch")
              , Input.option Client.Head (text "Head")
              , Input.option Client.Options (text "Options")
              ]
        }

joinTuple : String -> (String, String) -> String
joinTuple separator (key, value) =
    key ++ separator ++ value

headerView : Model a -> Element Msg
headerView model =
    let
        headersToText : Editable (List (String, String)) -> String
        headersToText eHeaders =
            editedOrNotEditedValue model.httpHeaders
                |> List.map (joinTuple ":")
                |> String.join "\n"
    in
        Input.multiline []
            { onChange = UpdateHeaders
            , text = headersToText model.httpHeaders
            , placeholder = Just <| Input.placeholder [] (text "Header: SomeHeader\nHeader2: SomeHeader2")
            , label = labelInputView "Headers: "
            , spellcheck = False
            }

bodyView : Model a -> Element Msg
bodyView model =
    Input.multiline []
        { onChange = SetHttpBody
        , text = editedOrNotEditedValue model.httpBody
        , placeholder = Just <| Input.placeholder [] (text "{}")
        , label = labelInputView "Body: "
        , spellcheck = False
        }

labelInputView : String -> Input.Label Msg
labelInputView labelText =
    let
        size =
            width (fill
                  |> maximum 100
                  |> minimum 100
                  )
    in
        Input.labelAbove [ centerY, size ] <| text labelText
