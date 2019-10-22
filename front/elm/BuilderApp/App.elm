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
                (RequestCollection id requestNodes) = model.requestCollection
                mFile = Maybe.andThen (BuilderTree.findFile requestNodes) model.selectedBuilderIndex
            in
                (model, Cmd.none)
                {-
                case (model.selectedBuilderIndex, mFile) of
                    (Just idx, Just file) ->
                        let
                            newBuilder = Builder.update subMsg file
                            newBuilderTree =
                                BuilderTree.modifyRequestNode (changeFileBuilder newBuilder) requestNodes idx
                            newModel =
                                { model
                                    | requestCollection = RequestCollection id newBuilderTree }
                        in
                            saveBuilder subMsg newModel

                    _ ->
                        (model, Cmd.none)-}

        ServerOk ->
            (model, Cmd.none)

        ServerError serverErrorMsg ->
            (model, Cmd.none)

saveBuilder : Builder.Msg -> Model a -> (Model a , Cmd Msg)
saveBuilder subMsg model =
    case subMsg of
        Builder.AskSave ->
            let
                (RequestCollection id requestNodes) = model.requestCollection
                backRequestCollection = Client.convertRequestNodesFromFrontToBack requestNodes
            in
                (model, Client.postRequestCollection "/" backRequestCollection fromServer)

        _ ->
            (model, Cmd.none)

fromServer : Result Http.Error a -> Msg
fromServer result =
    case result of
        Ok content ->
            ServerOk

        Err error ->
            ServerError <| httpErrorToString error

httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl s ->
            "bad url: " ++ s

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus status ->
            "bad status: " ++ String.fromInt status

        Http.BadBody response ->
            "bad payload: " ++ response
