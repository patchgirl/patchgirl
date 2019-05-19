module Builder.Response exposing (..)

import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Builder.Model exposing (Model)
import Builder.Message exposing (Msg)

view : Model -> Html Msg
view model =
  let
    status = case model.response of
      Just (Err error) ->
        case error of
          Http.BadUrl url -> "Bad url: [" ++ url ++ "]"
          Http.Timeout -> "Timeout"
          Http.NetworkError -> "Network error"
          Http.BadStatus statusCode -> "Error " ++ (String.fromInt statusCode)
          Http.BadBody body -> "Error " ++ (body)
      _ -> ""
    response = case model.response of
      Just (Ok responseBody) -> responseBody
      _ -> ""
  in
    div [ id "responseBody" ]
      [ div [] [ text status ]
      , textarea [ placeholder "response" ] [ text response ]
      ]
