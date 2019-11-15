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
import RequestRunner.Util as RequestRunner

update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case Debug.log "it"  msg of
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
                case subMsg of
                    Builder.AskRun ->
                        let
                            mEnvKeyValues = InitializedApplication.getEnvironmentKeyValuesToRun model
                        in
                            case (mEnvKeyValues, mFile) of
                                (envKeyValues, Just file) ->
                                    (model, buildRequestToRun envKeyValues model.varAppModel.vars file)
                                _ ->
                                    (model, Cmd.none)

                    _ ->
                        case (model.selectedBuilderIndex, mFile) of
                            (Just idx, Just file) ->
                                let
                                    newFile : File
                                    newFile =
                                        Builder.update subMsg file
                                    newBuilderTree =
                                        BuilderTree.modifyRequestNode (changeFileBuilder newFile) requestNodes idx
                                    newModel =
                                        { model
                                            | requestCollection = RequestCollection id newBuilderTree }
                                in
                                    (newModel, Cmd.none)
                                --saveBuilder subMsg newModel

                            _ ->
                                (model, Cmd.none)

        ServerOk _ ->
            (model, Cmd.none)

        ServerError serverErrorMsg ->
            (model, Cmd.none)

buildRequestToRun : List(String, String) -> List(String, String) -> File -> Cmd Msg
buildRequestToRun envKeyValues varKeyValues builder =
    let
        request = RequestRunner.buildRequest <| RequestRunner.buildRequestInput envKeyValues varKeyValues builder
        cmdRequest =
            { method = request.method
            , headers = request.headers
            , url = request.url
            , body = request.body
            , expect = Http.expectString ServerOk
            , timeout = Nothing
            , tracker = Nothing
            }
    in
        Http.request cmdRequest
