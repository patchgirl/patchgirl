module Builder.Response exposing (..)

import Http
import Html exposing (Html, Attribute, div, input, text, a, select, option, button, textarea, p)
import Html.Attributes exposing (value, placeholder, href, disabled)
import Html.Events exposing (onInput, onClick, keyCode, on)

import Builder.Message exposing (Msg)

type alias Model = Result Http.Error String

view : Maybe Model -> Html Msg
view mHttpResponse =
  let
    status = case mHttpResponse of
      Just (Err error) ->
        case error of
          Http.BadUrl url -> "Bad url: [" ++ url ++ "]"
          Http.Timeout -> "Timeout"
          Http.NetworkError -> "Network error"
          Http.BadStatus statusCode -> "Error " ++ (String.fromInt statusCode)
          Http.BadBody body -> "Error " ++ (body)
      _ -> ""
    response = case mHttpResponse of
      Just (Ok responseBody) -> responseBody
      _ -> ""
  in
    div []
      [ div [] [ text status ]
      , textarea [ placeholder "response" ] [ text response ]
      ]
