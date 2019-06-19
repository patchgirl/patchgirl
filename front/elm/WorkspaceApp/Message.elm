module WorkspaceApp.Message exposing (..)

type Msg
    = RenameWorkspace Int String
    | CreateWorkspace
    | DeleteWorkspace
