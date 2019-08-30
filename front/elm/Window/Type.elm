module Window.Type exposing (..)

-- TODO s/name/name
type alias Environment =
    { name : String
    , keyValues : List(String, String)
    }
