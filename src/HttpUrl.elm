module HttpUrl exposing (..)

import Html exposing (Html, Attribute, div, input, text, a, select, option, button, textarea, p)
import Html.Attributes exposing (value, placeholder, href, disabled)
import Html.Events exposing (onInput, onClick, keyCode, on)

import Message exposing (Msg(..))
import HttpMethod exposing (Model(..))
import HttpHeader
import HttpRequestValidity
import Json.Decode as Json
import Regex
import Url

type alias Model =
  { url : String
  , httpScheme : String
  , httpMethod : HttpMethod.Model
  , httpHeaders : List HttpHeader.Model
  }

fullUrl : Model -> String
fullUrl model =
  model.httpScheme ++ "://" ++ model.url

parseUrl : Model -> String -> Maybe Url.Url
parseUrl model url =
  let
    scheme = case model.httpScheme of
      "HTTP" -> "http"
      _ -> "https"
    urlRegex = Maybe.withDefault Regex.never <| Regex.fromString "(\\w+\\.\\w{2,}.*)|(localhost.*)"
    validUrl = Regex.contains urlRegex url
  in
    case validUrl of
      True -> Url.fromString <| scheme ++ "://" ++ url
      False -> Nothing

view : (Model, HttpRequestValidity.Model) -> Html Msg
view (model, httpRequestValidity) =
  let
    status = case httpRequestValidity.urlValid of
      False -> "Invalid Url"
      True -> "Send"
  in
    div []
      [ select [ onInput SetHttpMethod ] ([Get, Post, Put, Delete, Head, Patch, Options] |> List.map HttpMethod.toOption)
      , select [ onInput SetHttpScheme ]
        [ option [ Html.Attributes.value "HTTP" ] [ text "HTTP" ]
        , option [ Html.Attributes.value "HTTPS" ] [ text "HTTPS" ]
        ]
      , input [ placeholder "myApi.com/path?arg=someArg", value model.url, onInput UpdateUrl, onEnter RunHttpRequest ] []
      , button [onClick RunHttpRequest, disabled <| not httpRequestValidity.urlValid] [ text status ]
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
