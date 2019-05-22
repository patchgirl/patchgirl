module VarApp.App exposing (..)

import List.Extra as List

import VarApp.Model exposing (..)
import VarApp.Message exposing (Msg(..))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Delete idx ->
            (model, Cmd.none)
