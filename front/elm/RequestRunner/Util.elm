module RequestRunner.Util exposing (..)

import Http as Http
import Combine exposing (..)
import List.Extra as List

import Util.KeyValue.Model as KeyValue

import RequestRunner.Message exposing (..)
import RequestRunner.Model exposing (..)

import BuilderApp.Builder.Method as Builder
import BuilderApp.Builder.Util as Builder
import BuilderApp.Builder.Model as Builder

import RequestInput.Model as RequestInput

import VarApp.Model as VarApp
import Application.Type exposing (..)

buildRequestInput : List KeyValue -> List KeyValue -> Builder.Model a -> RequestInput.Model
buildRequestInput envKeyValues varKeyValues builder =
    { method = Builder.methodToString builder.httpMethod
    , headers = editedOrNotEditedValue builder.httpHeaders
    , url = interpolate envKeyValues varKeyValues (editedOrNotEditedValue builder.httpUrl)
    , body = editedOrNotEditedValue builder.httpBody
    }

buildRequest : RequestInput.Model -> Request
buildRequest requestInput =
    { method = requestInput.method
    , headers = List.map Builder.mkHeader requestInput.headers
    , url = requestInput.url
    , body = Http.stringBody "application/json" requestInput.body
    }

interpolate : List KeyValue -> List KeyValue -> String -> String
interpolate envKeys varKeyValues str =
  let
    build : TemplatedString -> String
    build templatedString =
      case templatedString of
        Sentence c -> c
        Key key ->
          case List.find (\envKey -> envKey.key == key) envKeys of
            Just { value } -> value
            Nothing ->
                case List.find (\varKeyValue -> varKeyValue.key == key) varKeyValues of
                    Just  { value } -> value
                    Nothing -> key
  in
    case toRaw str of
      Ok result -> List.map build result |> List.foldr (++) ""
      Err errors -> Debug.log errors str

toRaw : String -> Result String (List TemplatedString)
toRaw str =
  case parse templatedStringParser str of
    Ok (_, _, result) ->
      Ok result

    Err (_, stream, errors) ->
      Err (String.join " or " errors)

type TemplatedString
    = Sentence String
    | Key String

templatedStringParser : Parser s (List TemplatedString)
templatedStringParser = many (or keyParser anychar)

anychar : Parser s TemplatedString
anychar = regex "." |> map Sentence

keyParser : Parser s TemplatedString
keyParser =
  let
    envKey : Parser s TemplatedString
    envKey = regex "([a-zA-Z]|[0-9])+" |> map Key
  in
    between (string "{{") (string "}}") envKey
