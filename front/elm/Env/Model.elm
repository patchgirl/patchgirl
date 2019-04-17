module Env.Model exposing (..)

type alias KeyValue = (String, String)
type alias Model = List(String, String)

defaultModel = []

emptyModel = [("", "")]
