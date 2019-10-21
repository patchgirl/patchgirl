module BuilderApp.Builder.Model exposing (..)

import Http
import BuilderApp.Model as BuilderApp

type alias Model a =
    { a
        | url : String
        , method : BuilderApp.Method
        , headers : List(BuilderApp.Header)
        , body : String
        , response : Maybe BuilderApp.Response
    }

defaultBuilder =
  { url = ""
  , method = BuilderApp.Get
  , headers = []
  , body = ""
  , response = Nothing
  }

defaultModel1 =
  { url = "{{url}}/api/people/1"
  , method = BuilderApp.Get
  , headers = []
  , body = ""
  , response = Nothing
  }

defaultModel =
  { url = "swapi.co/api/people/2"
  , method = BuilderApp.Get
  , headers = []
  , body = ""
  , response = Nothing
  }
