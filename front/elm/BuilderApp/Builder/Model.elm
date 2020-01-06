module BuilderApp.Builder.Model exposing (..)

import Dict as Dict
import Application.Type exposing (..)
import Api.Generated as Client
import Http as Http

type alias RequestInput =
    { scheme : Scheme
    , method : Method
    , headers : List (String, String)
    , url : String
    , body : String
    }

type Scheme
    = Http
    | Https

type Method
    = Get
    | Post
    | Put
    | Delete
    | Patch
    | Head
    | Options


type RequestComputationResult
    = RequestTimeout
    | RequestNetworkError
    | RequestBadUrl
    | GotResponse Response

type alias Model a =
    { a
        | name : Editable String
        , httpUrl : Editable String
        , httpMethod : Editable Method
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

type alias Request =
    { method : Method
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    }

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
