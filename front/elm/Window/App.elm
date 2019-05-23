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

import EnvApp.View as EnvApp
import EnvApp.Model as EnvApp
import EnvApp.Message as EnvApp
import EnvApp.App as EnvApp

import EnvApp.EnvNav.View as EnvNav
import EnvApp.EnvNav.Model as EnvNav
import EnvApp.EnvNav.Message as EnvNav
import EnvApp.EnvNav.App as EnvNav
import EnvApp.EnvNav.Util as EnvNav

import BuilderApp.EnvSelection.View as EnvSelection
import BuilderApp.EnvSelection.Model as EnvSelection
import BuilderApp.EnvSelection.Message as EnvSelection
import BuilderApp.EnvSelection.App as EnvSelection

import RequestRunner.App as RequestRunner
import RequestRunner.Message as RequestRunner
import RequestRunner.Model as RequestRunner

import MainNavBar.Model as MainNavBar
import MainNavBar.View as MainNavBar
import MainNavBar.Message as MainNavBar
import MainNavBar.App as MainNavBar

import VarApp.Model as VarApp
import VarApp.View as VarApp
import VarApp.Message as VarApp
import VarApp.App as VarApp

import Window.View exposing(..)
import Window.Model exposing(..)

init : () -> (Model, Cmd Msg)
init _ =
  (defaultModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        BuilderTreeMsg subMsg ->
            case BuilderTree.update subMsg model.builderAppModel.builderTreeModel of
                (newBuilderTreeModel, newMsg) ->
                    let
                        formerBuilderAppModel = model.builderAppModel
                        newBuilderAppModel = { formerBuilderAppModel | builderTreeModel = newBuilderTreeModel }
                    in
                        ( { model | builderAppModel = newBuilderAppModel }
                        , Cmd.map BuilderTreeMsg newMsg
                        )

        EnvSelectionMsg subMsg ->
            case EnvSelection.update subMsg model.selectedEnvModel of
                (newSelectedEnvModel, newMsg) ->
                    ( { model | selectedEnvModel = newSelectedEnvModel }
                    , Cmd.map EnvSelectionMsg newMsg
                    )

        BuilderAppMsg subMsg ->
            case subMsg of
                BuilderApp.BuilderMsg (Builder.AskRun builder) ->
                    case EnvNav.getSelectedEnvInfo model.envNavModel of
                        Just envInfo ->
                            case RequestRunner.update (RequestRunner.Run envInfo.env model.varAppModel builder) Nothing of
                              (_, runnerSubMsg) -> (model, Cmd.map RequestRunnerMsg runnerSubMsg)

                        Nothing ->
                            (model, Cmd.none)
                _ ->
                   case BuilderApp.update subMsg model.builderAppModel of
                       (newBuilderTree, newMsg) ->
                           ( { model | builderAppModel = newBuilderTree }
                           , sendSaveTabRequest newBuilderTree.builderTreeModel
                           )

        BuilderMsg subMsg ->
            let
                mBuilder =
                    Maybe.andThen (BuilderTree.findBuilder model.builderAppModel.builderTreeModel.tree)
                        model.builderAppModel.builderTreeModel.selectedBuilderIndex
            in
                case (subMsg, mBuilder) of
                    (Builder.AskRun b, Just builder) ->
                        let
                            (updatedRequestRunner, cmdRequestRunner) =
                                (RequestRunner.update (RequestRunner.Run model.envModel model.varAppModel builder) model.runnerModel)
                        in
                            ( { model | runnerModel = updatedRequestRunner }, Cmd.map RequestRunnerMsg cmdRequestRunner)

                    (_, Just builder) ->
                        let
                            (updatedBuilder, cmdBuilder) = (Builder.update subMsg builder)
                        in
                            (model, Cmd.map BuilderMsg cmdBuilder)

                    _ ->
                        (model, Cmd.none)
      {-
      let
        mUpdatedBuilderToCmd : Maybe (Builder.Model, Cmd Builder.Msg)
        mUpdatedBuilderToCmd = Maybe.map (Builder.update subMsg) (builders model.builderAppModel)
      in
        case (mBuilder model.builderAppModel, subMsg) of
          (Nothing, _) -> (model, Cmd.none)
          (Just builder, Builder.AskRun) ->
            let
              (updatedRequestRunner, cmdRequestRunner) = (RequestRunner.update (RequestRunner.Run model.envModel builder) model.runnerModel)
            in
              ( { model | runnerModel = updatedRequestRunner }, Cmd.map RequestRunnerMsg cmdRequestRunner)
          (Just builder, _) ->
            let
              (updatedBuilder, cmdBuilder) = (Builder.update subMsg builder)
            in
              (model, Cmd.map BuilderMsg cmdBuilder)
-}

        SaveBuilderTreeResponse foo ->
            (model, Cmd.none)

        EnvNavMsg subMsg ->
            case EnvNav.update subMsg model.envNavModel of
                (newEnvNavModel, newMsg) ->
                    let
                        selectedEnvModel = model.selectedEnvModel
                        newSelectedEnvModel = { selectedEnvModel | envs = List.map .name newEnvNavModel.envs }
                    in
                        ( { model | envNavModel = newEnvNavModel, selectedEnvModel = newSelectedEnvModel }
                        , Cmd.map EnvNavMsg newMsg
                        )

        PostmanMsg subMsg ->
            case Postman.update subMsg model.postmanModel of
                (Just newBuilderTree, newMsg) ->
                    let
                        newBuilderTreeModel =
                            { selectedBuilderIndex = Nothing
                            , displayedBuilderIndexes = []
                            , tree = newBuilderTree
                            , displayedNodeMenuIndex = Nothing
                            }
                        formerBuilderAppModel = model.builderAppModel
                        newBuilderAppModel = { formerBuilderAppModel | builderTreeModel = newBuilderTreeModel }
                    in
                        ( { model | builderAppModel = newBuilderAppModel }, Cmd.map PostmanMsg newMsg)

                (Nothing, newMsg) -> (model, Cmd.none)


        RequestRunnerMsg subMsg ->
            (model, Cmd.none)
      {-
      case (subMsg, mBuilder model.builderAppModel, model.builderAppModel.displayedBuilderIndexes) of
        (RequestRunner.GetResponse response, Just builder, builderIndexes) ->
          let
            (updatedBuilder, cmdBuilder) = (Builder.update (Builder.GiveResponse response) builder)
            updateNode : BuilderTree.Node -> BuilderTree.Node
            updateNode oldNode =
              case oldNode of
                BuilderTree.File { name } -> BuilderTree.File { name = name, builder = updatedBuilder, showRenameInput = False }
                _ -> oldNode
            newBuilderTree = BuilderTree.modifyNode updateNode model.builderAppModel.tree 1 --builderIdx
            oldBuilderTreeModel = model.builderAppModel
            newBuilderTreeModel = { oldBuilderTreeModel | tree = newBuilderTree }
          in
            ( { model | builderAppModel = newBuilderTreeModel }, Cmd.map BuilderMsg cmdBuilder)
        _ -> (model, Cmd.none)-}

        EnvAppMsg subMsg ->
            case EnvApp.update subMsg model.envModel of
                (newEnvApp, newMsg) ->
                    ( { model | envModel = newEnvApp }, Cmd.map EnvAppMsg newMsg)

        MainNavBarMsg subMsg ->
            case MainNavBar.update subMsg model.mainNavBarModel of
                (newMainNavBarModel, newMsg) ->
                    ( { model | mainNavBarModel = newMainNavBarModel }
                    , Cmd.map MainNavBarMsg newMsg
                    )

        VarAppMsg subMsg ->
            case VarApp.update subMsg model.varAppModel of
                (newVarAppModel, newMsg) ->
                    ( { model | varAppModel = newVarAppModel }
                    , Cmd.map VarAppMsg newMsg
                    )

builders : BuilderTree.Model -> List (Maybe Builder.Model)
builders builderAppModel = List.map (BuilderTree.findBuilder builderAppModel.tree) builderAppModel.displayedBuilderIndexes

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
