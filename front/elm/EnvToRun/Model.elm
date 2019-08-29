module EnvToRun.Model exposing (..)

import Util.KeyValue.Model as KeyValue

type alias Model = List(KeyValue.Model)

defaultModel = []

emptyModel = [("", "")]
