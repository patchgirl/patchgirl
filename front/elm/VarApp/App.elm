module VarApp.App exposing (..)

import List.Extra as List

import VarApp.Model exposing (..)
import VarApp.Message exposing (Msg(..))

import Util.KeyValue.Model as KeyValue
import Util.KeyValue.Util as KeyValue

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PromptKey idx str ->
            let
                newVars = KeyValue.modify (KeyValue.changeKey str) idx model.vars
            in
                ( { model | vars = newVars }, Cmd.none)

        PromptValue idx str ->
            let
                newVars = KeyValue.modify (KeyValue.changeValue str) idx model.vars
            in
                ( { model | vars = newVars }, Cmd.none)

        AddNewInput ->
            let
                newVars = model.vars ++ emptyModel.vars
            in
                ( { model | vars = newVars }, Cmd.none)

        DeleteInput idx ->
            let
                newVars = KeyValue.delete idx model.vars
            in
                ( { model | vars = newVars }, Cmd.none)

        Drag idx ->
            ( { model | draggedId = Just idx }, Cmd.none)

        DragOver idx ->
            ( { model | overZoneId = Just idx }, Cmd.none)

        DragLeave ->
            ( { model | overZoneId = Nothing }, Cmd.none)

        DragEnd ->
            let
                newModel =
                    { model
                        | overZoneId = Nothing
                        , draggedId = Nothing
                    }
            in
                (newModel, Cmd.none)

        Drop newIdx ->
            let
                flippedGetAt : List a -> Int -> Maybe a
                flippedGetAt list i = List.getAt i list
                mModelToMove : Maybe KeyValue.Model
                mModelToMove = Maybe.andThen (flippedGetAt model.vars) model.draggedId
            in
                case (model.draggedId, mModelToMove) of
                    (Just modelToMoveId, Just modelToMove) ->
                        let
                            f : Int -> KeyValue.Model -> List(KeyValue.Model) -> List(KeyValue.Model)
                            f id elem acc =
                                case modelToMoveId == id of
                                    True -> Debug.log "acc" acc
                                    False ->
                                        case newIdx == id of
                                            True -> Debug.log "replace" <| acc ++ (
                                                     case id > modelToMoveId of
                                                         True -> [ elem, modelToMove ]
                                                         False -> [ modelToMove, elem ]
                                                    )
                                            False -> Debug.log "normal" <| acc ++ [ elem ]
                            newModel =
                                { model
                                    | overZoneId = Nothing
                                    , vars = Debug.log "newVars" <| List.indexedFoldl f [] model.vars
                                    , draggedId = Nothing
                                }
                        in (newModel, Cmd.none)
                    _ -> Debug.log "This should never happen" (model, Cmd.none)
