module Builder.Method exposing (..)

import Http
import Html exposing (Html, Attribute, div, input, text, a, select, option, button, textarea, p)
import Html.Attributes exposing (value, placeholder, href, disabled)
import Builder.Message exposing (Msg)
import Builder.Model exposing (Method(..), Model)

toString : Method -> String
toString method =
  case method of
    Get -> "GET"
    Post -> "POST"
    Put -> "PUT"
    Delete -> "DELETE"
    Patch -> "PATCH"
    Head -> "HEAD"
    _ -> "OPTIONS"

toOption : Method -> Html Msg
toOption method =
  option
    [ Html.Attributes.value (toString method) ]
    [ text (toString method) ]
