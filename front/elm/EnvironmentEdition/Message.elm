module EnvironmentEdition.Message exposing (..)

type Msg
  = SelectEnvToEdit Int
  | EnvironmentKeyValueEditionMsg KeyValueMsg
  | AskEnvironmentCreation String
  | EnvironmentCreated Int String
  | ChangeName Int String
  | AskRename Int String
  | EnvironmentRenamed Int String
  | AskDelete Int
  | EnvironmentDeleted Int
  | EnvServerError
  | ShowRenameInput Int

type KeyValueMsg
  = PromptKey Int String
  | PromptValue Int String
  | AddNewInput
  | AskSave
  | AskDeleteKeyValue Int
  | KeyDeleted Int
  | DeleteNewKeyValue Int
  | KeyValueServerError
