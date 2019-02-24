module EnvNav.Message exposing (..)

import Env.Message as Env

type Msg
  = Select Int
  | EnvMsg Int Env.Msg
