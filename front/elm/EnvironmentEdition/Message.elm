module EnvironmentEdition.Message exposing (..)

import EnvironmentKeyValueEdition.App as EnvironmentKeyValueEdition

type Msg
  = SelectEnvToEdit Int
  | EnvironmentKeyValueEditionMsg EnvironmentKeyValueEdition.Msg
  | Delete Int
  | Add
  | ShowRenameInput Int
  | Rename Int String
  | ChangeName Int String
