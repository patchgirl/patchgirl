module Tree.Message exposing (..)

import Tree.Model as Tree

type Msg
  = SetDisplayedBuilder Int
  | ToggleNode Int
  | Mkdir Int
  | Touch Int
  | ShowRenameInput Int
  | Rename Int String
  | Delete Int
  | ToggleMenu Int
