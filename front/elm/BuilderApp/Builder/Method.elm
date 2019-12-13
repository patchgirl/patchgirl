module BuilderApp.Builder.Method exposing (..)

import Http
import Html exposing (Html, Attribute, div, input, text, a, select, option, button, textarea, p)
import Html.Attributes exposing (value, placeholder, href, disabled)
import BuilderApp.Builder.Message exposing (..)
import BuilderApp.Builder.Model exposing (..)
import BuilderApp.Model as BuilderApp
import Api.Generated as Client

methodToString : Client.Method -> String
methodToString method =
  case method of
    Client.Get -> "GET"
    Client.Post -> "POST"
    Client.Put -> "PUT"
    Client.Delete -> "DELETE"
    Client.Patch -> "PATCH"
    Client.Head -> "HEAD"
    _ -> "OPTIONS"

toOption : Client.Method -> Html Msg
toOption method =
  option
    [ Html.Attributes.value (methodToString method) ]
    [ text (methodToString method) ]

fromString : String -> Maybe Client.Method
fromString method =
  case method of
    "GET" -> Just Client.Get
    "POST" -> Just Client.Post
    "PUT" -> Just Client.Put
    "DELETE" -> Just Client.Delete
    "PATCH" -> Just Client.Patch
    "HEAD" -> Just Client.Head
    "OPTIONS" -> Just Client.Options
    _ -> Nothing
