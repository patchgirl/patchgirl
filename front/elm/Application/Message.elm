module Application.Message exposing (..)

import Api.Client as Client
import InitializedApplication.Message as InitializedApplication
import BuilderApp.Model as BuilderApp

type Msg
  = ServerSuccess BuilderApp.RequestCollection
  | ServerError
  | InitializedApplicationMsg InitializedApplication.Msg
