module BuilderApp.BuilderTree.Message exposing (..)

type Msg
  = SetDisplayedBuilder Int
  | ToggleFolder Int
  | Mkdir Int
  | Touch Int
  | ShowRenameInput Int
  | ChangeName Int String -- while focus is on the input
  | AskRename Int String -- validate input
  | Rename Int String -- refresh input
  | Delete Int
  | ToggleMenu Int
  | DoNothing
  | BuilderTreeServerError
