module Window.Type exposing (..)

-- TODO s/environmentName/name
type alias Environment =
    { environmentName : String
    , keyValues : List(String, String)
    }
