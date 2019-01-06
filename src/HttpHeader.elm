module HttpHeader exposing (..)

import Maybe.Extra
import Http
import Html exposing (Html, Attribute, div, input, text, a, select, option, button, textarea, p)
import Html.Attributes exposing (value, placeholder, href, disabled)
import Html.Events exposing (onInput)
import HttpRequestValidity
import Message exposing (Msg(..))

type alias Model = (String, String)

parseHeaders : String -> Maybe (List(String, String))
parseHeaders headers =
  let
    parseRawHeader : String -> Maybe(String, String)
    parseRawHeader rawHeader =
      case String.split ":" rawHeader of
        [headerKey, headerValue] -> Just (headerKey, headerValue)
        _ -> Nothing
  in
    Maybe.Extra.traverse parseRawHeader (
      String.lines headers |> List.filter (not << String.isEmpty)
    )

mkHeader : Model -> Http.Header
mkHeader (headerKey, headerValue) = Http.header headerKey headerValue

view : HttpRequestValidity.Model -> Html Msg
view httpRequestValidity =
  let
    status = case httpRequestValidity.httpHeadersValid of
      False -> "Invalid Headers"
      True -> "Valid Headers"
  in
    div []
      [ textarea [ placeholder "Header: SomeHeader\nHeader2: SomeHeader2", onInput UpdateHeaders ] []
      , div [] [ text status ]
      ]
