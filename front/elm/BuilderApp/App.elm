module BuilderApp.App exposing (..)

import BuilderApp.Model exposing (..)
import BuilderApp.Message exposing (..)

import BuilderApp.BuilderTree.App as BuilderTree
import BuilderApp.BuilderTree.Util as BuilderTree
import BuilderApp.Builder.App as Builder
import BuilderApp.Util exposing (..)
import Util.Maybe as Maybe

update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
        DisplayBuilder idx ->
            let
                newModel = { model | selectedBuilderIndex = Just idx }
            in
                (newModel, Cmd.none)

        TreeMsg subMsg ->
            let
                newModel = BuilderTree.update subMsg model
            in
                (newModel, Cmd.none)

        BuilderMsg subMsg ->
            let
                mFile = Maybe.andThen (BuilderTree.findFile model.tree) model.selectedBuilderIndex
            in
                case (model.selectedBuilderIndex, mFile) of
                    (Just idx, Just file) ->
                        let
                            newBuilder = Builder.update subMsg file.builder
                            newBuilderTree = BuilderTree.modifyNode (changeFileBuilder newBuilder) model.tree idx
                            newModel = { model | tree = newBuilderTree }
                        in
                            (newModel, Cmd.none)

                    _ ->
                        (model, Cmd.none)
