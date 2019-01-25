module Runner.App exposing (..)

import Http

import Runner.Message exposing (..)
import Runner.Model exposing (..)

import Builder.Method as Builder
import Builder.Header as Builder
import Builder.Url as Builder

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Run env builder ->
      let
        httpRequest = Http.request
          { method = Builder.methodToString builder.method
          , headers = List.map Builder.mkHeader builder.headers
          , url = Builder.fullUrl builder
          , body = Http.emptyBody
          , expect = Http.expectString GetResponse
          , timeout = Nothing
          , tracker = Nothing
          }
      in (model, httpRequest)

    GetResponse _ ->
      (model, Cmd.none)
