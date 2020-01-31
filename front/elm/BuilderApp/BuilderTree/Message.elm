module BuilderApp.BuilderTree.Message exposing (..)

import Uuid

type Msg
  = SetDisplayedBuilder Int
  | ToggleFolder Int
  | RandomMkdir Int
  | Mkdir Int Uuid.Uuid
  | RandomTouch Int
  | Touch Int Uuid.Uuid
  | ShowRenameInput Int
  | ChangeName Int String -- while focus is on the input
  | AskRename Uuid.Uuid Int String -- validate input
  | Rename Int String -- refresh input
  | Delete Int
  | ToggleMenu Int
  | DoNothing
  | BuilderTreeServerError
