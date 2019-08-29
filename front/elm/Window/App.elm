module Window.App exposing (..)

import Window.Model exposing (..)
import Window.Message exposing (..)
import Window.StateHandler exposing (..)

import BuilderApp.App as BuilderApp
import BuilderApp.Model as BuilderApp
import BuilderApp.Message as BuilderApp
import BuilderApp.App as BuilderApp
import BuilderApp.Message as BuilderApp

import BuilderApp.Builder.App as Builder
import BuilderApp.Builder.Model as Builder
import BuilderApp.Builder.Message as Builder
import BuilderApp.Builder.App as Builder
import BuilderApp.Builder.Message as Builder

import BuilderApp.BuilderTree.View as BuilderTree
import BuilderApp.BuilderTree.Model as BuilderTree
import BuilderApp.BuilderTree.Message as BuilderTree
import BuilderApp.BuilderTree.App as BuilderTree

import Postman.View as Postman
import Postman.Model as Postman
import Postman.Message as Postman
import Postman.App as Postman

import EnvironmentEdition.View as EnvironmentEdition
import EnvironmentEdition.Model as EnvironmentEdition
import EnvironmentEdition.Message as EnvironmentEdition
import EnvironmentEdition.App as EnvironmentEdition

import EnvironmentEdition.EnvNav.View as EnvNav
import EnvironmentEdition.EnvNav.Message as EnvNav
import EnvironmentEdition.EnvNav.App as EnvNav
import EnvironmentEdition.EnvNav.Util as EnvNav

import BuilderApp.EnvSelection.Model as EnvSelection
import BuilderApp.EnvSelection.Message as EnvSelection
import BuilderApp.EnvSelection.App as EnvSelection

import BuilderApp.WorkspaceSelection.App as WorkspaceSelection

import RequestRunner.App as RequestRunner
import RequestRunner.Message as RequestRunner
import RequestRunner.Model as RequestRunner
import RequestRunner.Util as RequestRunner

import MainNavBar.Model as MainNavBar
import MainNavBar.View as MainNavBar
import MainNavBar.Message as MainNavBar
import MainNavBar.App as MainNavBar

import VarApp.Model as VarApp
import VarApp.View as VarApp
import VarApp.Message as VarApp
import VarApp.App as VarApp

import WorkspaceApp.Model as WorkspaceApp
import WorkspaceApp.View as WorkspaceApp
import WorkspaceApp.Message as WorkspaceApp
import WorkspaceApp.App as WorkspaceApp

import Util.Flip exposing (..)
import Util.List as List
import List.Extra as List

import Curl.Util as Curl

import Window.View exposing(..)
import Window.Model exposing(..)
import Window.Util exposing(..)

init : () -> (Model, Cmd Msg)
init _ =
  (defaultModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        BuilderTreeMsg subMsg ->
            let
                newBuildersAppModel newBuilderTree =
                    replaceBuilder model.selectedWorkspaceIndex model.buildersAppModel newBuilderTree
            in
                case getSelectedBuilder model of
                    Just builderApp ->
                        case BuilderTree.update subMsg builderApp.builderTreeModel of
                            newBuilderTreeModel ->
                                let
                                    formerBuildersAppModel = model.buildersAppModel
                                in
                                    ( { model | buildersAppModel = newBuildersAppModel { builderApp | builderTreeModel = newBuilderTreeModel } }
                                    , Cmd.none
                                    )
                    Nothing -> (model, Cmd.none)

        EnvSelectionMsg subMsg ->
            case EnvSelection.update subMsg (getEnvironmentNames model) model of
                newSelectedEnvModel ->
                    ( { model | selectedEnvironmentToRunIndex = newSelectedEnvModel.selectedEnvironmentToRunIndex }
                    , Cmd.none
                    )

        WorkspaceSelectionMsg subMsg ->
            case WorkspaceSelection.update subMsg model (getWorkspaceNames model) of
                newModel ->
                    (newModel, Cmd.none)

        BuilderAppMsg subMsg ->
            case subMsg of
                BuilderApp.BuilderMsg (Builder.AskRun builder) ->
                    case getEnvironmentToRun model of
                        Just environment ->
                            case RequestRunner.update (RequestRunner.Run environment.keyValues model.varAppModel builder) Nothing of
                              (_, runnerSubMsg) -> (model, Cmd.map RequestRunnerMsg runnerSubMsg)

                        Nothing ->
                            (model, Cmd.none)

                BuilderApp.BuilderMsg (Builder.ShowRequestAsCurl builder) ->
                    case getEnvironmentToRun model of
                        Just environment ->
                            let
                                requestInput = RequestRunner.buildRequestInput environment.keyValues model.varAppModel builder
                            in
                                case Debug.log "request" (Curl.showRequestAsCurl requestInput) of
                                    (_) -> (model, Cmd.none)

                        Nothing ->
                            (model, Cmd.none)

                _ ->
                    let
                        newBuildersAppModel newBuilder =
                            replaceBuilder model.selectedWorkspaceIndex model.buildersAppModel newBuilder
                    in
                        case getSelectedBuilder model of
                            Just builderApp ->
                                case BuilderApp.update subMsg builderApp of
                                    (newBuilderTree, newMsg) ->
                                        ( { model | buildersAppModel = newBuildersAppModel newBuilderTree }
                                        , sendSaveTabRequest newBuilderTree.builderTreeModel
                                        )
                            Nothing -> (model, Cmd.none)

        SaveBuilderTreeResponse foo ->
            (model, Cmd.none)

        EnvNavMsg subMsg ->
            case EnvNav.update subMsg model of
                newModel ->
                    (Debug.log "haha" newModel, Cmd.none)

        PostmanMsg subMsg ->
            case Postman.update subMsg model.postmanModel of
                (_, _) -> (model, Cmd.none)


        RequestRunnerMsg subMsg ->
            (model, Cmd.none)

        EnvironmentEditionMsg subMsg ->
            case EnvNav.getEnvironmentToEdit model of
                Just environment ->
                    case EnvironmentEdition.update subMsg environment of
                        newEnvironmentEdition ->
                            let
                                newModel = replaceEnvironmentToEdit model newEnvironmentEdition
                            in
                                (Debug.log "test" newModel, Cmd.none)

                Nothing ->
                    (model, Cmd.none)

        MainNavBarMsg subMsg ->
            case MainNavBar.update subMsg model.mainNavBarModel of
                newMainNavBarModel ->
                    ( { model | mainNavBarModel = newMainNavBarModel }
                    , Cmd.none
                    )

        VarAppMsg subMsg ->
            case VarApp.update subMsg model.varAppModel of
                newVarAppModel ->
                    ( { model | varAppModel = newVarAppModel }
                    , Cmd.none
                    )

        WorkspaceAppMsg subMsg ->
            case WorkspaceApp.update subMsg model.workspaces of
                newWorkspaceAppModel ->
                    ( { model | workspaces = newWorkspaceAppModel }
                    , Cmd.none
                    )

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
