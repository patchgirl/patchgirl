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
    column [ width fill ]
        [ urlView model
        , headerView model
        , bodyView
        , responseView model
        ]


urlView : Model a -> Element Msg
urlView model =
    column [ width fill ]
        [ row [ width fill ]
            [ el [ alignLeft, width fill ] <|
                  Input.text [ htmlAttribute <| Util.onEnter AskRun ]
                  { onChange = UpdateUrl
                  , text = editedOrNotEditedValue model.httpUrl
                  , placeholder = Just <| Input.placeholder [] (text "myApi.com/path?arg=someArg")
                  , label = Input.labelLeft [ centerY ] <| text "Url: "
                  }
            , row [ centerY
                  , height fill
                  , spacing 10
                  , alignRight
                  , paddingXY 20 0
                  , Font.color primaryColor
                  ] [
                   Input.button [ Border.solid
                           , Border.color secondaryColor
                           , Border.width 1
                           , Border.rounded 5
                           , Background.color secondaryColor
                           , height fill
                           , paddingXY 10 0
                           ]
                { onPress = Just <| AskRun
                , label = el [ centerY] <| iconWithTextAndColor "send" "Send" primaryColor
                }
            , Input.button [ Border.solid
                           , Border.color secondaryColor
                           , Border.width 1
                           , Border.rounded 5
                           , Background.color secondaryColor
                           , height fill
                           , paddingXY 10 0
                           ]
                { onPress = Just <| AskSave
                , label = el [ centerY] <| iconWithTextAndColor "save" "Save" primaryColor
                }
                  ]
            ]
        , Input.radioRow [ padding 10, spacing 20 ]
            { onChange = SetHttpMethod
            , selected = Just model.httpMethod
            , label = Input.labelLeft [ centerY ] (text "Method: ")
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
        ]

headerView : Model a -> Element Msg
headerView model = none {-
    div [ id "headersBuilder" ]
        [ textarea [ placeholder "Header: SomeHeader\nHeader2: SomeHeader2"
                   , onInput UpdateHeaders ] []
        ]-}

bodyView : Element Msg
bodyView = none{-
    div [ id "bodyBuilder" ]
        [ textarea [ placeholder "{ \n  \"your\": \"data\"\n}", onInput SetHttpBody ] []
        ]-}

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
