module VarApp.Model exposing (..)

import Util.KeyValue.Model as KeyValue

type alias Model =
    { vars : List(KeyValue.Model)
    , overZoneId : Maybe Int
    , draggedId : Maybe Int
    }

emptyModel : Model
emptyModel =
    { vars = [("", "")]
    , overZoneId = Nothing
    , draggedId = Nothing
    }
