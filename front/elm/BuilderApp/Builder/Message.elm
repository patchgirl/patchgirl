module BuilderApp.Builder.Message exposing (..)

import Http

import BuilderApp.Builder.Model exposing (..)
import BuilderApp.Model as BuilderApp
import Api.Client as Client

type Msg
  = UpdateUrl String
  | SetHttpMethod Client.Method
  | UpdateHeaders String
  | SetHttpBody String
  | AskRun
  | ShowRequestAsCurl
  | GiveResponse (Result Http.Error String)
  | AskSave
  | ServerOk  (Result Http.Error String)
