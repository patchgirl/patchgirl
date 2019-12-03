module BuilderApp.Builder.Model exposing (..)

import Http
import Api.Client as Client
import Application.Type exposing (..)
import Dict as Dict

type alias Model a =
    { a
        | name : Editable String
        , httpUrl : Editable String
        , httpMethod : Editable Client.Method
        , httpHeaders : Editable (List (String, String))
        , httpBody : Editable String
        , response : Maybe Response
        , showResponseView : Bool
    }

isBuilderDirty : Model a -> Bool
isBuilderDirty model =
    isDirty model.httpMethod ||
        isDirty model.httpHeaders ||
            List.any isDirty [model.name, model.httpUrl, model.httpBody]


type alias Response =
    { statusCode : Int
    , statusText : String
    , headers : Dict.Dict String String
    , body : String
    }

type Status
    = Success
    | Failure

type ErrorDetailed
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata String

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
