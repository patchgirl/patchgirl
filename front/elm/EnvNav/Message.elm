module EnvNav.Message exposing (..)

import EnvApp.Message as EnvApp

type Msg
  = Select Int
  | EnvAppMsg Int EnvApp.Msg
  | Delete Int
  | Add
  | ShowRenameInput Int
  | Rename Int String
