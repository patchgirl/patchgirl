module BuilderApp.BuilderTree.Message exposing (..)

import Uuid

type Msg
  = SetDisplayedBuilder Uuid.Uuid
  | ToggleFolder Uuid.Uuid
  | GenerateRandomUUIDForFolder Uuid.Uuid
  | AskMkdir Uuid.Uuid Uuid.Uuid
  | Mkdir Uuid.Uuid Uuid.Uuid
  | GenerateRandomUUIDForFile Uuid.Uuid
  | AskTouch Uuid.Uuid Uuid.Uuid
  | Touch Uuid.Uuid Uuid.Uuid
  | GenerateRandomUUIDForRootFile
  | AskTouchRoot Uuid.Uuid
  | TouchRoot Uuid.Uuid
  | ShowRenameInput Uuid.Uuid
  | ChangeName Uuid.Uuid String -- while focus is on the input
  | AskRename Uuid.Uuid String  -- validate input
  | Rename Uuid.Uuid String     -- refresh input
  | AskDelete Uuid.Uuid
  | Delete Uuid.Uuid
  | ToggleMenu Uuid.Uuid
  | DoNothing
  | BuilderTreeServerError
