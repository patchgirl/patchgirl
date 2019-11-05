module BuilderApp.Builder.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import BuilderApp.Builder.Message exposing (..)
import BuilderApp.Builder.Model exposing (..)
import Util.View as Util
import BuilderApp.Builder.Method exposing (..)
import Api.Client as Client
import Http

view : Model a -> Html Msg
view model =
    div [ id "builder" ]
        [ urlView model
        , headerView model
        , bodyView
        , responseView model
        ]

urlView : Model a -> Html Msg
urlView model =
    div [ id "urlBuilder" ]
        [ select [ class "urlOption", onInput SetHttpMethod ]
              ([ Client.Get
               , Client.Post
               , Client.Put
               , Client.Delete
               , Client.Head
               , Client.Patch
               , Client.Options
               ] |> List.map toOption)
        , input [ id "urlInput"
                , placeholder "myApi.com/path?arg=someArg"
                , value model.httpUrl
                , onInput UpdateUrl
                , Util.onEnter AskRun ] []
        , button [ onClick AskRun ] [ text "Send" ]
        , button [ onClick ShowRequestAsCurl] [ text "Curl" ]
        , button [ onClick AskSave] [ text "Save" ]
        ]

headerView : Model a -> Html Msg
headerView model =
    div [ id "headersBuilder" ]
        [ textarea [ placeholder "Header: SomeHeader\nHeader2: SomeHeader2"
                   , onInput UpdateHeaders ] []
        ]

bodyView : Html Msg
bodyView =
    div [ id "bodyBuilder" ]
        [ textarea [ placeholder "{ \n  \"your\": \"data\"\n}", onInput SetHttpBody ] []
        ]

responseView : Model a -> Html Msg
responseView model =
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
