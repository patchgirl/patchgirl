module Tree.Message exposing (..)

import Tree.Model as Tree

type Msg
  = SetSelectedNode Tree.Node
  | SetDisplayedBuilder Int
  | ToggleNode Int
  | Mkdir Int
  | Touch Int
  | ShowRenameInput Int
  | Rename Int
