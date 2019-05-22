module RequestRunner.Util exposing (..)

import Http
import Combine exposing (..)
import List.Extra as List

import RequestRunner.Message exposing (..)
import RequestRunner.Model exposing (..)

import Builder.Method as Builder
import Builder.Header as Builder
import Builder.Url as Builder

import Builder.Model as Builder
import Env.Model as Env

buildRequest : Env.Model -> Builder.Model -> Cmd Msg
buildRequest env builder =
  let
    httpRequest = Http.request
      { method = Builder.methodToString builder.method
      , headers = List.map Builder.mkHeader builder.headers
      , url = interpolate env (Builder.fullUrl builder)
      , body = Http.emptyBody
      , expect = Http.expectString GetResponse
      , timeout = Nothing
      , tracker = Nothing
      }
  in
    httpRequest

interpolate : Env.Model -> String -> String
interpolate env str =
  let
    build : Sentence -> String
    build sentence =
      case sentence of
        Characters c -> c
        EnvKey envKey ->
          case List.find (\(key, value) -> key == envKey) env of
            Just (_, value) -> value
            Nothing -> envKey
  in
    case toRaw str of
      Ok result -> List.map build result |> List.foldr (++) ""
      Err errors -> Debug.log errors str

toRaw : String -> Result String (List Sentence)
toRaw str =
  case parse match str of
    Ok (_, _, result) ->
      Ok result

    Err (_, stream, errors) ->
      Err (String.join " or " errors)

type Sentence
  = Characters String
  | EnvKey String

match : Parser s (List Sentence)
match = many (or envPlaceholder anychar)

anychar : Parser s Sentence
anychar = regex "." |> map Characters

envPlaceholder : Parser s Sentence
envPlaceholder =
  let
    envKey : Parser s Sentence
    envKey = regex "[a-zA-Z]+" |> map EnvKey
  in
    between (string "{{") (string "}}") envKey
