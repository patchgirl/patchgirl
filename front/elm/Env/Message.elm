module Env.Message exposing (..)

type Msg
  = PromptKey Int String
  | PromptValue Int String
  | AddNewInput
  | DeleteInput Int