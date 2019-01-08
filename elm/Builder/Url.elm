module Builder.Url exposing (..)

import Html exposing (Html, Attribute, div, input, text, a, select, option, button, textarea, p)
import Html.Attributes exposing (value, placeholder, href, disabled, id)
import Html.Events exposing (onInput, onClick, keyCode, on)

import Json.Decode as Json
import Regex
import Url as ElmUrl

import Builder.Message exposing (Msg(..))
import Builder.Body
import Builder.Header
import Builder.Model exposing (Model, Method(..))
import Builder.Method

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
  let
    status = case model.validity.url of
      False -> "Invalid Url"
      True -> "Send"
  in
    div [ id "urlBuilder" ]
      [ select [ onInput SetHttpMethod ] ([Get, Post, Put, Delete, Head, Patch, Options] |> List.map Builder.Method.toOption)
      , select [ onInput SetHttpScheme ]
        [ option [ Html.Attributes.value "HTTP" ] [ text "HTTP" ]
        , option [ Html.Attributes.value "HTTPS" ] [ text "HTTPS" ]
        ]
      , input [ id "urlInput", placeholder "myApi.com/path?arg=someArg", value model.url, onInput Builder.Message.UpdateUrl, onEnter Builder.Message.RunHttpRequest ] []
      , button [onClick RunHttpRequest, disabled <| not model.validity.url] [ text status ]
      ]

onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    isEnter code =
      if code == 13 then
        Json.succeed msg
      else
        Json.fail "not ENTER"
    in
      on "keydown" (Json.andThen isEnter keyCode)
