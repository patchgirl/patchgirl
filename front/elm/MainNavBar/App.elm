module MainNavBar.App exposing (..)

import MainNavBar.Message exposing (..)
import MainNavBar.Model exposing (..)

update : Msg -> Model -> Model
update msg model =
    case msg of
        OpenReqTab ->
            ReqTab
        OpenEnvTab ->
            EnvTab
        OpenVarTab ->
            VarTab
