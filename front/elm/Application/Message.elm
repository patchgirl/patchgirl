module Application.Message exposing (..)

import Api.Client as Client
import InitializedApplication.Message as InitializedApplication

type Msg
  = ServerSuccess (List Client.RequestNode)
  | ServerError
  | InitializedApplicationMsg InitializedApplication.Msg
