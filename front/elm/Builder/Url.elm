module Builder.Url exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, keyCode, on)

import Json.Decode as Json
import Regex
import Url as ElmUrl

import Builder.Message exposing (Msg(..))
import Builder.Body
import Builder.Header
import Builder.Model exposing (Model, Method(..))
import Builder.Method

import Util.View as Util

fullUrl : Model -> String
fullUrl model =
  model.scheme ++ "://" ++ model.url

parseUrl : Model -> String -> Maybe ElmUrl.Url
parseUrl model url =
  let
    scheme = case model.scheme of
      "HTTP" -> "http"
      _ -> "https"
    urlRegex = Maybe.withDefault Regex.never <| Regex.fromString "(\\w+\\.\\w{2,}.*)|(localhost.*)"
    validUrl = Regex.contains urlRegex url
  in
    case validUrl of
      True -> ElmUrl.fromString <| scheme ++ "://" ++ url
      False -> Nothing

view : Model -> Html Msg
view model =
  div [ id "urlBuilder" ]
    [ select [ class "urlOption", onInput SetHttpMethod ] ([Get, Post, Put, Delete, Head, Patch, Options] |> List.map Builder.Method.toOption)
    , select [ onInput SetHttpScheme ]
      [ option [ Html.Attributes.value "HTTP" ] [ text "HTTP" ]
      , option [ Html.Attributes.value "HTTPS" ] [ text "HTTPS" ]
      ]
    , input [ id "urlInput"
            , placeholder "myApi.com/path?arg=someArg"
            , value model.url
            , onInput Builder.Message.UpdateUrl
            , Util.onEnter (AskRun model) ] []
    , button [ onClick (AskRun model) ] [ text "Send" ]
    ]
