module MainNavBar.Model exposing (..)

type Model
    = ReqTab
    | EnvTab
    | VarTab
    | WorkspaceTab

defaultModel = EnvTab
