module Tab.App exposing (..)

import Tab.Message exposing (..)
import Tab.Model exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OpenReqWindow -> (ReqTab, Cmd.none)
    OpenEnvWindow -> (EnvTab, Cmd.none)
