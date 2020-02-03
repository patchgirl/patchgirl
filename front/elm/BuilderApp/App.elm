module BuilderApp.App exposing (..)

import BuilderApp.Model exposing (..)
import BuilderApp.Message exposing (..)

import BuilderApp.BuilderTree.App as BuilderTree
import BuilderApp.BuilderTree.Util as BuilderTree
import BuilderApp.Builder.App as Builder
import BuilderApp.Util exposing (..)
import Util.Maybe as Maybe
import Api.Generated as Client
import Api.Converter as Client
import Http as Http
import EnvironmentToRunSelection.App as EnvSelection
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

        TreeMsg subMsg ->
            let
                (newModel, newSubMsg) = BuilderTree.update subMsg model
            in
                (newModel, Cmd.map TreeMsg newSubMsg)

        BuilderMsg subMsg ->
            let
                (RequestCollection requestCollectionId requestNodes) = model.requestCollection
                mFile : Maybe File
                mFile = Maybe.andThen (BuilderTree.findFile requestNodes) model.selectedBuilderIndex
            in
                case (model.selectedBuilderIndex, mFile) of
                    (Just id, Just file) ->
                        let
                            (newFile, newSubMsg) =
                                Builder.update subMsg (InitializedApplication.getEnvironmentKeyValuesToRun model) model.varAppModel.vars file

                            newBuilderTree =
                                List.map (BuilderTree.modifyRequestNode2 id (changeFileBuilder newFile)) requestNodes

                            newModel =
                                { model
                                    | requestCollection = RequestCollection requestCollectionId newBuilderTree
                                }
                        in
                            (newModel, Cmd.map BuilderMsg newSubMsg)

                    _ ->
                        (model, Cmd.none)
