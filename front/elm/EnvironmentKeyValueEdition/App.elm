module EnvironmentKeyValueEdition.App exposing (..)

import EnvironmentKeyValueEdition.Model exposing (..)
import EnvironmentKeyValueEdition.Message exposing (Msg(..))

import Util.KeyValue.Util as KeyValue
import Window.Type as Type

update : Msg -> Type.Environment -> Type.Environment
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
            let
                newKeyValues = model.keyValues ++ emptyModel
            in
                { model | keyValues = newKeyValues }

        DeleteKeyValue idx ->
            let
                newKeyValues = KeyValue.delete idx model.keyValues
            in
                { model | keyValues = Debug.log "tai" newKeyValues }
