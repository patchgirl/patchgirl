module Builder.Message exposing (..)

import Http

type Msg
  = UpdateUrl String
  | SetHttpMethod String
  | RunHttpRequest
  | UpdateHeaders String
  | SetHttpScheme String
  | GetHttpResponse (Result Http.Error String)
  | SetHttpBody String
  | AskRun
  | GiveResponse (Result Http.Error String)
