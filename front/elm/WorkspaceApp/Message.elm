module WorkspaceApp.Message exposing (..)

type Msg
    = RenameWorkspace Int String
    | AddNewInput
    | DeleteWorkspace Int
