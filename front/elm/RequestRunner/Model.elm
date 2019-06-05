module RequestRunner.Model exposing (..)

import Http as Http

type alias Model = Maybe Request

type alias Request =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    }
