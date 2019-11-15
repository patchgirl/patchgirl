module BuilderApp.Builder.View exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Events as Events
import Element.Input as Input

import Icon exposing (..)
import Color exposing (..)

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
                , responseView model
                ]
    in
        case model.showResponseView of
            False ->
                builderView

            True ->
                row []
                    [ builderView
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

headerView : Model a -> Element Msg
headerView model =
    let
        untuple : (String, String) -> String
        untuple (key, value) =
            case String.isEmpty key of
                True -> ""
                False -> key ++ ":" ++ value

        headersToText : Editable (List (String, String)) -> String
        headersToText eHeaders =
            editedOrNotEditedValue model.httpHeaders
                |> List.map untuple
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

responseView : Model a -> Element Msg
responseView model = none {-
    let
        status =
            case model.response of
                Just (Err error) ->
                    case error of
                        Http.BadUrl url -> "Bad url: [" ++ url ++ "]"
                        Http.Timeout -> "Timeout"
                        Http.NetworkError -> "Network error"
                        Http.BadStatus statusCode -> "Error " ++ (String.fromInt statusCode)
                        Http.BadBody body -> "Error " ++ (body)
                _ -> ""
        response =
            case model.response of
                Just (Ok responseBody) -> responseBody
                _ -> ""
    in
        div [ id "responseBody" ]
            [ div [] [ text status ]
            , textarea [ placeholder "response" ] [ text response ]
            ]
-}

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
