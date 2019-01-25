module Builder.Message exposing (..)

import Http

type Msg
  = UpdateUrl String
  | SetHttpMethod String
  | UpdateHeaders String
  | SetHttpScheme String
  | SetHttpBody String
  | AskRun
  | GiveResponse (Result Http.Error String)
