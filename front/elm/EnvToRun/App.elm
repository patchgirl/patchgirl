module EnvToRun.App exposing (..)

import EnvToRun.Model exposing (..)
import EnvToRun.Message exposing (Msg(..))

import Util.KeyValue.Util as KeyValue

type alias Environment =
    { environmentName : String
    , keyValues : List(String, String)
    }

update : Msg -> Environment -> Environment
update msg model =
    case msg of
        PromptKey idx str ->
            let
                newKeyValues = KeyValue.modify (KeyValue.changeKey str) idx model.keyValues
            in
                { model | keyValues = newKeyValues }

        PromptValue idx str ->
            let
                newKeyValues = KeyValue.modify (KeyValue.changeValue str) idx model.keyValues
            in
                { model | keyValues = newKeyValues }

        AddNewInput ->
            { model | keyValues = model.keyValues ++ emptyModel }

        DeleteInput idx ->
            let
                newKeyValues = KeyValue.delete idx model.keyValues
            in
                { model | keyValues = newKeyValues }
