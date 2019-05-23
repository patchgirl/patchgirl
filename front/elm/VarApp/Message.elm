module VarApp.Message exposing (..)

type Msg
    = PromptKey Int String
    | PromptValue Int String
    | AddNewInput
    | DeleteInput Int
