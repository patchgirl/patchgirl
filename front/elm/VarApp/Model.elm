module VarApp.Model exposing (..)

type alias KeyValue = (String, String)
type alias Model = List(String, String)

emptyModel : Model
emptyModel = [("", "")]
