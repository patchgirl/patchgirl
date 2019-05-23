module EnvApp.Model exposing (..)

import Util.KeyValue.Model exposing (..)

type alias Model = List(KeyValue)

defaultModel = []

emptyModel = [("", "")]
