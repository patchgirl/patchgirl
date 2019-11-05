module BuilderApp.Builder.Model exposing (..)

import Http
import BuilderApp.Model as BuilderApp
import Api.Client as Client

type alias Model a =
    { a
        | httpUrl : String
        , httpMethod : Client.Method
        , httpHeaders : List(BuilderApp.Header)
        , httpBody : String
        , response : Maybe BuilderApp.Response
    }

defaultBuilder =
  { httpUrl = ""
  , httpMethod = Client.Get
  , httpHeaders = []
  , httpBody = ""
  , response = Nothing
  }

defaultModel1 =
  { httpUrl = "{{url}}/api/people/1"
  , httpMethod = Client.Get
  , httpHeaders = []
  , httpBody = ""
  , response = Nothing
  }

defaultModel =
  { httpUrl = "swapi.co/api/people/2"
  , httpMethod = Client.Get
  , httpHeaders = []
  , httpBody = ""
  , response = Nothing
  }
