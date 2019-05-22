module Window.App exposing (..)

import Window.Model exposing (..)
import Window.Message exposing (..)
import Window.StateHandler exposing (..)

import Builder.App as Builder
import Builder.Model as Builder
import Builder.Message as Builder

import Builders.App as Builders
import Builders.Message as Builders

import Tree.View as Tree
import Tree.Model as Tree
import Tree.Message as Tree
import Tree.App as Tree

import Postman.View as Postman
import Postman.Model as Postman
import Postman.Message as Postman
import Postman.App as Postman

import Env.View as Env
import Env.Model as Env
import Env.Message as Env
import Env.App as Env

import EnvNav.View as EnvNav
import EnvNav.Model as EnvNav
import EnvNav.Message as EnvNav
import EnvNav.App as EnvNav
import EnvNav.Util as EnvNav

import EnvSelection.View as EnvSelection
import EnvSelection.Model as EnvSelection
import EnvSelection.Message as EnvSelection
import EnvSelection.App as EnvSelection

import Runner.App as Runner
import Runner.Message as Runner
import Runner.Model as Runner

import MainNavBar.Model as MainNavBar
import MainNavBar.View as MainNavBar
import MainNavBar.Message as MainNavBar
import MainNavBar.App as MainNavBar

import Window.View exposing(..)
import Window.Model exposing(..)

init : () -> (Model, Cmd Msg)
init _ =
  (defaultModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TreeMsg subMsg ->
      case Tree.update subMsg model.treeModel of
        (newTreeModel, newMsg) -> ( { model | treeModel = newTreeModel }, Cmd.map TreeMsg newMsg)

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

    EnvSelectionMsg subMsg ->
      case EnvSelection.update subMsg model.selectedEnvModel of
        (newSelectedEnvModel, newMsg) ->
          ( { model | selectedEnvModel = newSelectedEnvModel }, Cmd.map EnvSelectionMsg newMsg)

    PostmanMsg subMsg ->
      case Postman.update subMsg model.postmanModel of
        (Just newTree, newMsg) ->
          let
            newTreeModel =
              { selectedBuilderIndex = Nothing
              , displayedBuilderIndexes = []
              , tree = newTree
              , displayedNodeMenuIndex = Nothing
              }
          in
            ( { model | treeModel = newTreeModel }, Cmd.map PostmanMsg newMsg)

        (Nothing, newMsg) ->
          (model, Cmd.none)

    BuilderMsg subMsg ->
      let
        mBuilder = Debug.log "mbuilder" Maybe.andThen (Tree.findBuilder model.treeModel.tree) model.treeModel.selectedBuilderIndex
      in
        case (subMsg, mBuilder) of
          (Builder.AskRun b, Just builder) ->
            let
              (updatedRunner, cmdRunner) = (Runner.update (Runner.Run model.envModel builder) model.runnerModel)
            in
              ( { model | runnerModel = updatedRunner }, Cmd.map RunnerMsg cmdRunner)

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
        mUpdatedBuilderToCmd = Maybe.map (Builder.update subMsg) (builders model.treeModel)
      in
        case (mBuilder model.treeModel, subMsg) of
          (Nothing, _) -> (model, Cmd.none)
          (Just builder, Builder.AskRun) ->
            let
              (updatedRunner, cmdRunner) = (Runner.update (Runner.Run model.envModel builder) model.runnerModel)
            in
              ( { model | runnerModel = updatedRunner }, Cmd.map RunnerMsg cmdRunner)
          (Just builder, _) ->
            let
              (updatedBuilder, cmdBuilder) = (Builder.update subMsg builder)
            in
              (model, Cmd.map BuilderMsg cmdBuilder)
-}
    RunnerMsg subMsg ->
      {-
      case (subMsg, mBuilder model.treeModel, model.treeModel.displayedBuilderIndexes) of
        (Runner.GetResponse response, Just builder, builderIndexes) ->
          let
            (updatedBuilder, cmdBuilder) = (Builder.update (Builder.GiveResponse response) builder)
            updateNode : Tree.Node -> Tree.Node
            updateNode oldNode =
              case oldNode of
                Tree.File { name } -> Tree.File { name = name, builder = updatedBuilder, showRenameInput = False }
                _ -> oldNode
            newTree = Tree.modifyNode updateNode model.treeModel.tree 1 --builderIdx
            oldTreeModel = model.treeModel
            newTreeModel = { oldTreeModel | tree = newTree }
          in
            ( { model | treeModel = newTreeModel }, Cmd.map BuilderMsg cmdBuilder)
        _ -> (model, Cmd.none)-}
      (model, Cmd.none)

    EnvMsg subMsg ->
      case Env.update subMsg model.envModel of
        (newEnv, newMsg) ->
          ( { model | envModel = newEnv }, Cmd.map EnvMsg newMsg)

    MainNavBarMsg subMsg ->
        case MainNavBar.update subMsg model.mainNavBarModel of
            (newMainNavBarModel, newMsg) ->
                ( { model | mainNavBarModel = newMainNavBarModel }
                , Cmd.map MainNavBarMsg newMsg
                )

    BuildersMsg subMsg ->
      case subMsg of
        Builders.BuilderMsg (Builder.AskRun builder) ->
          case EnvNav.getSelectedEnvInfo model.envNavModel of
            Just envInfo ->
              case Runner.update (Runner.Run envInfo.env builder) Nothing of
                (_, runnerSubMsg) -> (model, Cmd.map RunnerMsg runnerSubMsg)

            Nothing ->
              (model, Cmd.none)
        _ ->
          case Builders.update subMsg model.treeModel of
            (newTree, newMsg) ->
              ( { model | treeModel = newTree }, sendSaveTabRequest newTree)

    SaveTreeResponse foo ->
      (model, Cmd.none)

builders : Tree.Model -> List (Maybe Builder.Model)
builders treeModel = List.map (Tree.findBuilder treeModel.tree) treeModel.displayedBuilderIndexes

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
