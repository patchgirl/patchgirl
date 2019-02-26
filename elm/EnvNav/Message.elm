module EnvNav.Message exposing (..)

import Env.Message as Env

type Msg
  = Select Int
  | EnvMsg Int Env.Msg
  | Delete Int
  | Add
  | ShowRenameInput Int
  | Rename Int String
