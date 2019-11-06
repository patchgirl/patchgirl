module BuilderApp.BuilderTree.Message exposing (..)

type Msg
  = SetDisplayedBuilder Int
  | ToggleFolder Int
  | Mkdir Int
  | Touch Int
  | ShowRenameInput Int
  | Rename Int String
  | ChangeName Int String
  | Delete Int
  | ToggleMenu Int
  | DoNothing
