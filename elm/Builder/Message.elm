module Builder.Message exposing (..)

import Http

import Builder.Model exposing (..)

type Msg
  = UpdateUrl String
  | SetHttpMethod String
  | UpdateHeaders String
  | SetHttpScheme String
  | SetHttpBody String
  | AskRun Model
  | GiveResponse (Result Http.Error String)
