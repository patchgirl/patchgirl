module BuilderApp.Builder.Model exposing (..)

import Dict as Dict
import Application.Type exposing (..)
import Api.Generated as Client
import Http as Http


type RequestComputationResult
    = RequestTimeout
    | RequestNetworkError
    | RequestBadUrl
    | GotResponse Response

type alias Model a =
    { a
        | name : Editable String
        , httpUrl : Editable String
        , httpMethod : Editable Client.Method
        , httpHeaders : Editable (List (String, String))
        , httpBody : Editable String
        , requestComputationResult : Maybe RequestComputationResult
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
  }

defaultModel1 =
  { httpUrl = NotEdited "{{url}}/api/people/1"
  , httpMethod = Client.Get
  , httpHeaders = NotEdited []
  , httpBody = NotEdited ""
  }

defaultModel =
  { httpUrl = NotEdited "swapi.co/api/people/2"
  , httpMethod = Client.Get
  , httpHeaders = NotEdited []
  , httpBody = NotEdited ""
  }
