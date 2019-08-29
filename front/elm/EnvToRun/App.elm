module EnvToRun.App exposing (..)

import EnvToRun.Model exposing (..)
import EnvToRun.Message exposing (Msg(..))

import Util.KeyValue.Util as KeyValue

update : Msg -> Model -> Model
update msg model =
    case msg of
        PromptKey idx str ->
            KeyValue.modify (KeyValue.changeKey str) idx model

        PromptValue idx str ->
            KeyValue.modify (KeyValue.changeValue str) idx model

        AddNewInput ->
            model ++ emptyModel

        DeleteInput idx ->
            KeyValue.delete idx model
