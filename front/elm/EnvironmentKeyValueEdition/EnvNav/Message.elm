module EnvironmentKeyValueEdition.EnvNav.Message exposing (..)

import EnvironmentKeyValueEdition.Message as EnvironmentKeyValueEdition

type Msg
  = SelectEnvToEdit Int
  | EnvironmentKeyValueEditionMsg Int EnvironmentKeyValueEdition.Msg
  | Delete Int
  | Add
  | ShowRenameInput Int
  | Rename Int String
