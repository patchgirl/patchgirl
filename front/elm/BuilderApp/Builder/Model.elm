module BuilderApp.Builder.Model exposing (..)

import Http
import BuilderApp.Model as BuilderApp
import Api.Client as Client
import Application.Type exposing (..)

type alias Model a =
    { a
        | httpUrl : Editable String
        , httpMethod : Client.Method
        , httpHeaders : Editable (List BuilderApp.Header)
        , httpBody : String
        , response : Maybe BuilderApp.Response
    }

defaultBuilder =
  { httpUrl = NotEdited ""
  , httpMethod = Client.Get
  , httpHeaders = NotEdited []
  , httpBody = ""
  , response = Nothing
  }

defaultModel1 =
  { httpUrl = NotEdited "{{url}}/api/people/1"
  , httpMethod = Client.Get
  , httpHeaders = NotEdited []
  , httpBody = ""
  , response = Nothing
  }

defaultModel =
  { httpUrl = NotEdited "swapi.co/api/people/2"
  , httpMethod = Client.Get
  , httpHeaders = NotEdited []
  , httpBody = ""
  , response = Nothing
  }
