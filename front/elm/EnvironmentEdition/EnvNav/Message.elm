module EnvironmentEdition.EnvNav.Message exposing (..)

import EnvironmentEdition.Message as EnvironmentEdition

type Msg
  = SelectEnvToEdit Int
  | EnvironmentEditionMsg Int EnvironmentEdition.Msg
  | Delete Int
  | Add
  | ShowRenameInput Int
  | Rename Int String
