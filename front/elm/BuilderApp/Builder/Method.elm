module BuilderApp.Builder.Method exposing (..)

import Http
import Html exposing (Html, Attribute, div, input, text, a, select, option, button, textarea, p)
import Html.Attributes exposing (value, placeholder, href, disabled)
import BuilderApp.Builder.Message exposing (..)
import BuilderApp.Builder.Model exposing (..)
import BuilderApp.Model as BuilderApp

methodToString : BuilderApp.Method -> String
methodToString method =
  case method of
    BuilderApp.Get -> "GET"
    BuilderApp.Post -> "POST"
    BuilderApp.Put -> "PUT"
    BuilderApp.Delete -> "DELETE"
    BuilderApp.Patch -> "PATCH"
    BuilderApp.Head -> "HEAD"
    _ -> "OPTIONS"

toOption : BuilderApp.Method -> Html Msg
toOption method =
  option
    [ Html.Attributes.value (methodToString method) ]
    [ text (methodToString method) ]

fromString : String -> Maybe BuilderApp.Method
fromString method =
  case method of
    "GET" -> Just BuilderApp.Get
    "POST" -> Just BuilderApp.Post
    "PUT" -> Just BuilderApp.Put
    "DELETE" -> Just BuilderApp.Delete
    "PATCH" -> Just BuilderApp.Patch
    "HEAD" -> Just BuilderApp.Head
    "OPTIONS" -> Just BuilderApp.Options
    _ -> Nothing
