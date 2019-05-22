module MainNavBar.App exposing (..)

import MainNavBar.Message exposing (..)
import MainNavBar.Model exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OpenReqWindow -> (ReqTab, Cmd.none)
    OpenEnvWindow -> (EnvTab, Cmd.none)
