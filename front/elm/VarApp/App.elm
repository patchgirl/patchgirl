module VarApp.App exposing (..)

import List.Extra as List

import VarApp.Model exposing (..)
import VarApp.Message exposing (Msg(..))

import Util.KeyValue.Model as KeyValue
import Util.KeyValue.Util as KeyValue
import Util.List as List

update : Msg -> Model -> Model
update msg model =
    case msg of
        PromptKey idx str ->
            let
                newVars = KeyValue.modify (KeyValue.changeKey str) idx model.vars
            in
                { model | vars = newVars }

        PromptValue idx str ->
            let
                newVars = KeyValue.modify (KeyValue.changeValue str) idx model.vars
            in
                { model | vars = newVars }

        AddNewInput ->
            let
                newVars = model.vars ++ emptyModel.vars
            in
                { model | vars = newVars }

        DeleteInput idx ->
            let
                newVars = KeyValue.delete idx model.vars
            in
                { model | vars = newVars }

        Drag idx ->
            { model | draggedId = Just idx }

        DragOver idx ->
            { model | overZoneId = Just idx }

        DragLeave ->
            { model | overZoneId = Nothing }

        DragEnd ->
            let
                newModel =
                    { model
                        | overZoneId = Nothing
                        , draggedId = Nothing
                    }
            in
                newModel

        Drop newIdx ->
            let
                mModelToMove : Maybe KeyValue.Model
                mModelToMove = Maybe.andThen (\i -> List.getAt i model.vars) model.draggedId
            in
                case (model.draggedId, mModelToMove) of
                    (Just modelToMoveId, Just modelToMove) ->
                        let
                            newModel =
                                { model
                                    | overZoneId = Nothing
                                    , vars = List.changePlace newIdx modelToMoveId modelToMove model.vars
                                    , draggedId = Nothing
                                }
                        in
                            newModel
                    _ -> Debug.log "This should never happen" model
