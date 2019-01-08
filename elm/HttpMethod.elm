module HttpMethod exposing (..)

import Http
import Html exposing (Html, Attribute, div, input, text, a, select, option, button, textarea, p)
import Html.Attributes exposing (value, placeholder, href, disabled)
import Message exposing (Msg(..))

type Model = Get | Post | Put | Delete | Patch | Head | Options

toString : Model -> String
toString httpMethod =
  case httpMethod of
    Get -> "GET"
    Post -> "POST"
    Put -> "PUT"
    Delete -> "DELETE"
    Patch -> "PATCH"
    Head -> "HEAD"
    _ -> "OPTIONS"

toOption : Model -> Html Msg
toOption httpMethod = option
                        [ Html.Attributes.value (toString httpMethod) ]
                        [ text <| toString httpMethod ]
