module BuilderApp.Builder.View exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input

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
    column []
        [ urlView model
        , headerView model
        , bodyView
        , responseView model
        ]


urlView : Model a -> Element Msg
urlView model =
    column []
        [ Input.radioRow [ padding 10, spacing 20 ]
              { onChange = SetHttpMethod
              , selected = Just model.httpMethod
              , label = Input.labelAbove [] (text "Method")
              , options =
                    [ Input.option Client.Get (text "Get")
                    , Input.option Client.Post (text "Post")
                    , Input.option Client.Put (text "Put")
                    ]
              }
        , Input.text [ htmlAttribute <| Util.onEnter AskRun ]
            { onChange = UpdateUrl
            , text = editedOrNotEditedValue model.httpUrl
            , placeholder = Just <| Input.placeholder [] (text "myApi.com/path?arg=someArg")
            , label = Input.labelAbove [] <| text "Url"
            }


              {-input [ id "urlInput"
                , placeholder "myApi.com/path?arg=someArg"
                , value model.httpUrl
                , onInput UpdateUrl
                , Util.onEnter AskRun ] []-}
        ]
            {-
        , button [ onClick AskRun ] [ text "Send" ]
        , button [ onClick ShowRequestAsCurl] [ text "Curl" ]
        , button [ onClick AskSave] [ text "Save" ]
        ]-}

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
