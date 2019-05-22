module BuilderTree.Message exposing (..)

import BuilderTree.Model as BuilderTree

type Msg
  = SetDisplayedBuilder Int
  | ToggleNode Int
  | Mkdir Int
  | Touch Int
  | ShowRenameInput Int
  | Rename Int String
  | Delete Int
  | ToggleMenu Int
