module BuilderApp.Builder.Message exposing (..)

import Http

import BuilderApp.Builder.Model exposing (..)
import BuilderApp.Model as BuilderApp
import Api.Generated as Client

type Msg
  = UpdateUrl String
  | SetHttpMethod Client.Method
  | UpdateHeaders String
  | SetHttpBody String
  | SetHttpBodyResponse String
  | AskRun
  | ServerOk (Result ErrorDetailed ( Http.Metadata, String ))
  | AskSave
