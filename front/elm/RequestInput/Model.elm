module RequestInput.Model exposing (..)

type alias Model =
    { method : String
    , headers : List (String, String)
    , url : String
    , body : String
    }
