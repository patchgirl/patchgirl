module BuilderApp.BuilderTree.Message exposing (..)

type Msg
  = SetDisplayedBuilder Int
  | ToggleFolder Int
  | Mkdir Int
  | Touch Int
  | ShowRenameInput Int
  | Rename Int String
  | Delete Int
  | ToggleMenu Int
