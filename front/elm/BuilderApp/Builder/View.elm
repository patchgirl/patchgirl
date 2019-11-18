module BuilderApp.Builder.View exposing (..)

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
import BuilderApp.Builder.Method exposing (..)
import Api.Client as Client
import Http
import Application.Type exposing (..)
import Dict as Dict
import Json.Print as Json

view : Model a -> Element Msg
view model =
    let
        builderView =
            column [ width fill, spacing 10 ]
                [ column [ width fill ]
                      [ row [ width fill, spacing 10 ]
                            [ urlView model
                            , mainActionButtonsView
                            ]
                      ]
                , methodView model
                , headerView model
                , bodyView model
                ]
    in
        case model.showResponseView of
            False ->
                el [ width fill ] builderView

            True ->
                row [ width fill, spacing 20 ]
                    [ el [ width (fillPortion 5), alignTop ] builderView
                    , el [ width (fillPortion 5), alignTop ] (responseView model)
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
            case response of
                Response status metadata body ->
                    let
                        statusText =
                            String.fromInt metadata.statusCode ++ " - " ++ metadata.statusText
                    in
                        if metadata.statusCode >= 200 && metadata.statusCode < 300 then
                            labelSuccess statusText
                        else if metadata.statusCode >= 400 && metadata.statusCode < 500 then
                            labelWarning statusText
                        else if metadata.statusCode >= 500 then
                            labelError statusText
                        else
                            labelWarning statusText

        headersResponseView : Response -> Element Msg
        headersResponseView response =
            let
                (Response _ metadata _) = response
                headers =
                    Dict.toList metadata.headers
                        |> List.map (joinTuple ": ")
                        |> String.join "\n"
            in
                Input.multiline []
                    { onChange = SetHttpBody
                    , text = headers
                    , placeholder = Nothing
                    , label = labelView "Headers: "
                    , spellcheck = False
                    }

        bodyResponseView : Response -> Element Msg
        bodyResponseView response =
            case response of
                Response status metadata body ->
                    case body of
                        "" ->
                            none

                        _ ->
                            Input.multiline []
                                { onChange = SetHttpBody
                                , text = bodyResponseText body metadata.headers
                                , placeholder = Nothing
                                , label = labelView "body: "
                                , spellcheck = False
                                }
    in
        case model.response of
            Nothing ->
                none

            Just (_ as response) ->
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
            , label = labelView "Url: "
            }

mainActionButtonsView : Element Msg
mainActionButtonsView =
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
            , Background.color secondaryColor
            , height fill
            , paddingXY 10 0
            ]
    in
        row rowParam
            [ Input.button inputParam
                { onPress = Just <| AskRun
                , label = el [ centerY] <| iconWithTextAndColor "send" "Send" primaryColor
                }
            , Input.button inputParam
                { onPress = Just <| AskSave
                , label = el [ centerY] <| iconWithTextAndColor "save" "Save" primaryColor
                }
            ]

methodView : Model a -> Element Msg
methodView model =
    Input.radioRow [ padding 10, spacing 20 ]
        { onChange = SetHttpMethod
        , selected = Just model.httpMethod
        , label = labelView "Method: "
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
                |> List.map (joinTuple ": ")
                |> String.join "\n"
    in
        Input.multiline []
            { onChange = UpdateHeaders
            , text = headersToText model.httpHeaders
            , placeholder = Just <| Input.placeholder [] (text "Header: SomeHeader\nHeader2: SomeHeader2")
            , label = labelView "Headers: "
            , spellcheck = False
            }

bodyView : Model a -> Element Msg
bodyView model =
    Input.multiline []
        { onChange = SetHttpBody
        , text = editedOrNotEditedValue model.httpBody
        , placeholder = Just <| Input.placeholder [] (text "{}")
        , label = labelView "Body: "
        , spellcheck = False
        }

labelView : String -> Input.Label Msg
labelView labelText =
    let
        size =
            width (fill
                  |> maximum 100
                  |> minimum 100
                  )
    in
        Input.labelLeft [ centerY, size ] <| text labelText
