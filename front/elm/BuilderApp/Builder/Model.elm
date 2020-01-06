module BuilderApp.Builder.Model exposing (..)

import Dict as Dict
import Application.Type exposing (..)
import Api.Generated as Client
import Http as Http


-- * model

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
            List.any isDirty [ model.name
                             , model.httpUrl
                             , model.httpBody
                             ]



-- * request computation input


type alias RequestComputationInput =
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


-- * request computation result


type RequestComputationResult
    = RequestTimeout
    | RequestNetworkError
    | RequestBadUrl
    | GotRequestComputationOutput RequestComputationOutput

type ErrorDetailed
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata String


-- * request computation output


type alias RequestComputationOutput =
    { statusCode : Int
    , statusText : String
    , headers : Dict.Dict String String
    , body : String
    }
