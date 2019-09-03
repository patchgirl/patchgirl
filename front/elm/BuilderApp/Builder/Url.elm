module BuilderApp.Builder.Url exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode as Json
import Regex
import Url as ElmUrl

import BuilderApp.Builder.Message exposing (Msg(..))
import BuilderApp.Builder.Body
import BuilderApp.Builder.Header
import BuilderApp.Builder.Model exposing (Model, Method(..))
import BuilderApp.Builder.Method as Builder

import Util.View as Util

parseUrl : Model -> String -> Maybe ElmUrl.Url
parseUrl model url =
  let
    urlRegex = Maybe.withDefault Regex.never <| Regex.fromString "(\\w+\\.\\w{2,}.*)|(localhost.*)"
    validUrl = Regex.contains urlRegex url
  in
    case validUrl of
      True -> ElmUrl.fromString url
      False -> Nothing

view : Model -> Html Msg
view model =
  div [ id "urlBuilder" ]
    [ select [ class "urlOption", onInput SetHttpMethod ]
          ([Get, Post, Put, Delete, Head, Patch, Options] |> List.map Builder.toOption)
    , input [ id "urlInput"
            , placeholder "myApi.com/path?arg=someArg"
            , value model.url
            , onInput UpdateUrl
            , Util.onEnter (AskRun model) ] []
    , button [ onClick (AskRun model) ] [ text "Send" ]
    , button [ onClick (ShowRequestAsCurl model)] [ text "Curl" ]
    , button [ onClick AskSave] [ text "Save" ]
    ]
