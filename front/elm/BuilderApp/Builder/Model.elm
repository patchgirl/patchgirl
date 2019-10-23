module BuilderApp.Builder.Model exposing (..)

import Http
import BuilderApp.Model as BuilderApp
import Api.Client as Client

type alias Model a =
    { a
        | url : String
        , method : Client.Method
        , headers : List(BuilderApp.Header)
        , body : String
        , response : Maybe BuilderApp.Response
    }

defaultBuilder =
  { url = ""
  , method = Client.Get
  , headers = []
  , body = ""
  , response = Nothing
  }

defaultModel1 =
  { url = "{{url}}/api/people/1"
  , method = Client.Get
  , headers = []
  , body = ""
  , response = Nothing
  }

defaultModel =
  { url = "swapi.co/api/people/2"
  , method = Client.Get
  , headers = []
  , body = ""
  , response = Nothing
  }
