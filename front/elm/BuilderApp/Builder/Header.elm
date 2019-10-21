module BuilderApp.Builder.Header exposing (..)

import Maybe.Extra
import Http
import Html exposing (Html, Attribute, div, input, text, a, select, option, button, textarea, p)
import Html.Attributes exposing (value, placeholder, href, disabled, id)
import Html.Events exposing (onInput)
import BuilderApp.Model as BuilderApp
import BuilderApp.Builder.Message exposing (..)
import BuilderApp.Builder.Model exposing (..)

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

mkHeader : BuilderApp.Header -> Http.Header
mkHeader (headerKey, headerValue) = Http.header headerKey headerValue

view : Model a -> Html Msg
view model =
  div [ id "headersBuilder" ]
    [ textarea [ placeholder "Header: SomeHeader\nHeader2: SomeHeader2"
               , onInput UpdateHeaders ] [] ]
