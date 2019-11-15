module BuilderApp.App exposing (..)

import BuilderApp.Model exposing (..)
import BuilderApp.Message exposing (..)

import BuilderApp.BuilderTree.App as BuilderTree
import BuilderApp.BuilderTree.Util as BuilderTree
import BuilderApp.Builder.App as Builder
import BuilderApp.Util exposing (..)
import Util.Maybe as Maybe
import BuilderApp.Builder.Message as Builder
import Api.Client as Client
import Api.Converter as Client
import Http as Http
import EnvironmentToRunSelection.App as EnvSelection
import RequestInput.Model as RequestInput
import BuilderApp.Builder.Model as Builder
import BuilderApp.Builder.Util as Builder
import BuilderApp.Builder.Method as Builder
import List.Extra as List
import InitializedApplication.Model as InitializedApplication

update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
        EnvSelectionMsg idx ->
            let
                newModel =
                    { model | selectedEnvironmentToRunIndex = Just idx }
            in
                (newModel, Cmd.none)

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
                (RequestCollection id requestNodes) = model.requestCollection
                mFile : Maybe File
                mFile = Maybe.andThen (BuilderTree.findFile requestNodes) model.selectedBuilderIndex
            in
                case (model.selectedBuilderIndex, mFile) of
                    (Just idx, Just file) ->
                        let
                            (newFile, newSubMsg) =
                                Builder.update subMsg (InitializedApplication.getEnvironmentKeyValuesToRun model) model.varAppModel.vars file
                            newBuilderTree =
                                BuilderTree.modifyRequestNode (changeFileBuilder newFile) requestNodes idx
                            newModel =
                                { model
                                    | requestCollection = RequestCollection id newBuilderTree
                                }
                        in
                            (newModel, Cmd.map BuilderMsg newSubMsg)

                    _ ->
                        (model, Cmd.none)
