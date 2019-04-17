module Runner.App exposing (..)

import Runner.Message exposing (..)
import Runner.Model exposing (..)
import Runner.Util exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Run env builder ->
      (model, buildRequest env builder)

    GetResponse _ ->
      (model, Cmd.none)
