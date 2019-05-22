module RequestRunner.App exposing (..)

import RequestRunner.Message exposing (..)
import RequestRunner.Model exposing (..)
import RequestRunner.Util exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Run envs vars builder ->
      (model, buildRequest envs vars builder)

    GetResponse _ ->
      (model, Cmd.none)
