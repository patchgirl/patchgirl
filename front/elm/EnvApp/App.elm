module EnvApp.App exposing (..)

import EnvApp.Model exposing (..)
import EnvApp.Message exposing (Msg(..))

import Util.KeyValue.Util as KeyValue

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PromptKey idx str ->
            (KeyValue.modify (KeyValue.changeKey str) idx model, Cmd.none)

        PromptValue idx str ->
            (KeyValue.modify (KeyValue.changeValue str) idx model, Cmd.none)

        AddNewInput ->
            (model ++ emptyModel, Cmd.none)

        DeleteInput idx ->
            (KeyValue.delete idx model, Cmd.none)
