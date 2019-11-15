module BuilderApp.Builder.Model exposing (..)

import Http
import Api.Client as Client
import Application.Type exposing (..)

type alias Model a =
    { a
        | httpUrl : Editable String
        , httpMethod : Client.Method
        , httpHeaders : Editable (List (String, String))
        , httpBody : Editable String
        , response : Maybe (Result ErrorDetailed (Http.Metadata, String))
        , showResponseView : Bool
    }

type ErrorDetailed
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata String
    | BadBody String

defaultBuilder =
  { httpUrl = NotEdited ""
  , httpMethod = Client.Get
  , httpHeaders = NotEdited []
  , httpBody = NotEdited ""
  , response = Nothing
  }

defaultModel1 =
  { httpUrl = NotEdited "{{url}}/api/people/1"
  , httpMethod = Client.Get
  , httpHeaders = NotEdited []
  , httpBody = NotEdited ""
  , response = Nothing
  }

defaultModel =
  { httpUrl = NotEdited "swapi.co/api/people/2"
  , httpMethod = Client.Get
  , httpHeaders = NotEdited []
  , httpBody = NotEdited ""
  , response = Nothing
  }
