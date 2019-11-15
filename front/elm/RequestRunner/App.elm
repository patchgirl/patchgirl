module RequestRunner.App exposing (..)

import Http as Http

import RequestRunner.Message exposing (..)
import RequestRunner.Model exposing (..)
import RequestRunner.Util exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Run envs vars ->
        (model, Cmd.none)

    GetResponse _ ->
      (model, Cmd.none)
