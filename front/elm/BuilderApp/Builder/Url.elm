module BuilderApp.Builder.Url exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode as Json
import Regex
import Url as ElmUrl

import BuilderApp.Builder.Message exposing (..)
import BuilderApp.Builder.Body exposing (..)
import BuilderApp.Builder.Header exposing (..)
import BuilderApp.Builder.Method exposing (..)
import BuilderApp.Builder.Model exposing (..)
import BuilderApp.Model as BuilderApp

import Util.View as Util

parseUrl : Model a -> String -> Maybe ElmUrl.Url
parseUrl model url =
  let
    urlRegex = Maybe.withDefault Regex.never <| Regex.fromString "(\\w+\\.\\w{2,}.*)|(localhost.*)"
    validUrl = Regex.contains urlRegex url
  in
    case validUrl of
      True -> ElmUrl.fromString url
      False -> Nothing

view : Model a -> Html Msg
view model =
  div [ id "urlBuilder" ]
    [ select [ class "urlOption", onInput SetHttpMethod ]
          ([ BuilderApp.Get
          , BuilderApp.Post
          , BuilderApp.Put
          , BuilderApp.Delete
          , BuilderApp.Head
          , BuilderApp.Patch
          , BuilderApp.Options
          ] |> List.map toOption)
    , input [ id "urlInput"
            , placeholder "myApi.com/path?arg=someArg"
            , value model.url
            , onInput UpdateUrl
            , Util.onEnter AskRun ] []
    , button [ onClick AskRun ] [ text "Send" ]
    , button [ onClick ShowRequestAsCurl] [ text "Curl" ]
    , button [ onClick AskSave] [ text "Save" ]
    ]
