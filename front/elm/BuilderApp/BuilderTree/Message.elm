module BuilderApp.BuilderTree.Message exposing (..)

import Uuid

type Msg
  = SetDisplayedBuilder Uuid.Uuid
  | ToggleFolder Uuid.Uuid
  | ToggleMenu Uuid.Uuid
  -- mkdir
  | GenerateRandomUUIDForFolder Uuid.Uuid
  | AskMkdir Uuid.Uuid Uuid.Uuid
  | Mkdir Uuid.Uuid Uuid.Uuid
  -- create file
  | GenerateRandomUUIDForFile Uuid.Uuid
  | AskTouch Uuid.Uuid Uuid.Uuid
  | Touch Uuid.Uuid Uuid.Uuid
  -- create root file
  | GenerateRandomUUIDForRootFile
  | AskTouchRoot Uuid.Uuid
  | TouchRoot Uuid.Uuid
  -- create root folder
  | GenerateRandomUUIDForRootFolder
  | AskMkdirRoot Uuid.Uuid
  | MkdirRoot Uuid.Uuid
  -- rename
  | ShowRenameInput Uuid.Uuid
  | ChangeName Uuid.Uuid String -- while focus is on the input
  | AskRename Uuid.Uuid String  -- validate input
  | Rename Uuid.Uuid String     -- refresh input
  -- delete
  | AskDelete Uuid.Uuid
  | Delete Uuid.Uuid
  | DoNothing
  | BuilderTreeServerError
