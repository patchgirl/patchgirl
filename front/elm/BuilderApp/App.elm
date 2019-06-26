module BuilderApp.App exposing (..)

import BuilderApp.Model exposing (..)
import BuilderApp.Message exposing (..)

import BuilderApp.BuilderTree.Model as BuilderTree
import BuilderApp.BuilderTree.Util as BuilderTree
import BuilderApp.Builder.App as Builder
import BuilderApp.Util exposing (..)
import Util.Maybe as Maybe

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectTab idx ->
        let
            formerBuilderTreeModel = model.builderTreeModel
            newBuilderTreeModel = { formerBuilderTreeModel | selectedBuilderIndex = Just idx }
        in
            ( { model | builderTreeModel = newBuilderTreeModel }
            , Cmd.none)

    CloseTab idx ->
      let
        newDisplayedBuilderIndexes = List.filter (\x -> x /= idx) model.builderTreeModel.displayedBuilderIndexes
        newSelectedBuilderIndex =
          case Maybe.exists model.builderTreeModel.selectedBuilderIndex ((==) idx) of
            False -> model.builderTreeModel.selectedBuilderIndex
            True -> findPrevious model.builderTreeModel.displayedBuilderIndexes idx
        formerBuilderTreeModel = model.builderTreeModel
        newBuilderTreeModel =
            { formerBuilderTreeModel
                | displayedBuilderIndexes = newDisplayedBuilderIndexes
                , selectedBuilderIndex = newSelectedBuilderIndex
            }
      in
        ( { model | builderTreeModel = newBuilderTreeModel }
        , Cmd.none)

    SaveTab idx ->
      let
        newBuilderTree = BuilderTree.modifyNode markFileAsSaved model.builderTreeModel.tree idx
        formerBuilderTreeModel = model.builderTreeModel
        newBuilderTreeModel = { formerBuilderTreeModel | tree = newBuilderTree }
      in
        ({ model | builderTreeModel = newBuilderTreeModel }
        , Cmd.none)

    BuilderMsg subMsg ->
      let
        mBuilder = Maybe.andThen (BuilderTree.findBuilder model.builderTreeModel.tree) model.builderTreeModel.selectedBuilderIndex
      in
        case (model.builderTreeModel.selectedBuilderIndex, mBuilder) of
          (Just idx, Just builder) ->
            let
              (newBuilder, cmdBuilder) = (Builder.update subMsg builder)
              newBuilderTree = BuilderTree.modifyNode (changeFileBuilder newBuilder) model.builderTreeModel.tree idx
              formerBuilderTreeModel = model.builderTreeModel
              newBuilderTreeModel = { formerBuilderTreeModel | tree = newBuilderTree }
            in
              ( { model | builderTreeModel = newBuilderTreeModel }
              , Cmd.map BuilderMsg cmdBuilder)

          _ ->
            (model, Cmd.none)
