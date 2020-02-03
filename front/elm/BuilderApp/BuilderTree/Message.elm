module BuilderApp.BuilderTree.Message exposing (..)

import Uuid

type Msg
  = SetDisplayedBuilder Int
  | ToggleFolder Int
  | GenerateRandomUUIDForFolder Int Uuid.Uuid
  | AskMkdir Int Uuid.Uuid Uuid.Uuid
  | Mkdir Int Uuid.Uuid
  | AskTouch Int Uuid.Uuid
  | Touch Int Uuid.Uuid
  | GenerateRandomUUIDForFile Int
  | ShowRenameInput Int
  | ChangeName Int String -- while focus is on the input
  | AskRename Uuid.Uuid Int String -- validate input
  | Rename Int String -- refresh input
  | AskDelete Uuid.Uuid
  | Delete Uuid.Uuid
  | ToggleMenu Int
  | DoNothing
  | BuilderTreeServerError
