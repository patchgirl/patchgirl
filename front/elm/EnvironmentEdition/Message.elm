module EnvironmentEdition.Message exposing (..)

import EnvironmentKeyValueEdition.App as EnvironmentKeyValueEdition

type Msg
  = SelectEnvToEdit Int
  | EnvironmentKeyValueEditionMsg EnvironmentKeyValueEdition.Msg
  | Delete Int
  | AskEnvironmentCreation String
  | EnvironmentCreated Int String
  | ChangeName Int String
  | AskRename Int String
  | EnvironmentUpdated Int String
  | AskDelete Int
  | EnvironmentDeleted Int
  | ServerError
  | Add
  | ShowRenameInput Int
