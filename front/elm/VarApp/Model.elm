module VarApp.Model exposing (..)

import Util.KeyValue.Model exposing (..)

type alias Model = List(KeyValue)

emptyModel : Model
emptyModel = [("", "")]
