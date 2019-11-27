module EnvironmentEdition.Message exposing (..)

import EnvironmentKeyValueEdition.App as EnvironmentKeyValueEdition

type Msg
  = SelectEnvToEdit Int
  | EnvironmentKeyValueEditionMsg EnvironmentKeyValueEdition.Msg
  | AskEnvironmentCreation String
  | EnvironmentCreated Int String
  | ChangeName Int String
  | AskRename Int String
  | EnvironmentRenamed Int String
  | AskDelete Int
  | EnvironmentDeleted Int
  | ServerError
  | Add
  | ShowRenameInput Int
