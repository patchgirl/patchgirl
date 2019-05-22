module RequestRunner.App exposing (..)

import RequestRunner.Message exposing (..)
import RequestRunner.Model exposing (..)
import RequestRunner.Util exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Run env builder ->
      (model, buildRequest env builder)

    GetResponse _ ->
      (model, Cmd.none)
