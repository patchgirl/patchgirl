module MainNavBar.App exposing (..)

import MainNavBar.Message exposing (..)
import MainNavBar.Model exposing (..)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        OpenReqTab -> (ReqTab, Cmd.none)
        OpenEnvTab -> (EnvTab, Cmd.none)
        OpenVarTab -> (VarTab, Cmd.none)
        OpenWorkspaceTab -> (WorkspaceTab, Cmd.none)
