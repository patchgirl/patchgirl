module BuilderApp.Builder.Message exposing (..)

import Http

import BuilderApp.Builder.Model exposing (..)

type Msg
  = UpdateUrl String
  | SetHttpMethod String
  | UpdateHeaders String
  | SetHttpBody String
  | AskRun Model
  | ShowRequestAsCurl Model
  | GiveResponse (Result Http.Error String)
  | AskSave
