module BuilderApp.Builder.Message exposing (..)

import Http

import BuilderApp.Builder.Model exposing (..)
import BuilderApp.Model as BuilderApp

type Msg
  = UpdateUrl String
  | SetHttpMethod String
  | UpdateHeaders String
  | SetHttpBody String
  | AskRun
  | ShowRequestAsCurl
  | GiveResponse (Result Http.Error String)
  | AskSave
