module EnvToRun.EnvNav.Message exposing (..)

import EnvToRun.Message as EnvToRun

type Msg
  = SelectEnvToEdit Int
  | EnvToRunMsg Int EnvToRun.Msg
  | Delete Int
  | Add
  | ShowRenameInput Int
  | Rename Int String
