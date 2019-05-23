module BuilderApp.App exposing (..)

import BuilderApp.Model exposing (..)
import BuilderApp.Message exposing (..)

import BuilderApp.BuilderTree.Model as BuilderTree
import BuilderApp.BuilderTree.Util as BuilderTree
import BuilderApp.Builder.App as Builder

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectTab idx ->
        let
            formerBuilderAppModel = model.builderTreeModel
            newBuilderAppModel = { formerBuilderAppModel | selectedBuilderIndex = Just idx }
        in
            ( { model | builderTreeModel = newBuilderAppModel }, Cmd.none)

    CloseTab idx ->
      let
        findPrevious : List a -> a -> Maybe a
        findPrevious l a =
          case l of
            x :: y :: z ->
              if a == x then
                Just y
              else if a == y then
                Just x
              else
                findPrevious (y :: z) a
            x :: xs -> findPrevious xs a
            [] -> Nothing

        newDisplayedBuilderIndexes = List.filter (\x -> x /= idx) model.builderTreeModel.displayedBuilderIndexes
        newSelectedBuilderIndex =
          case Just idx == model.builderTreeModel.selectedBuilderIndex of
            False -> model.builderTreeModel.selectedBuilderIndex
            True -> findPrevious model.builderTreeModel.displayedBuilderIndexes idx
        formerBuilderAppModel = model.builderTreeModel
        newBuilderAppModel = { formerBuilderAppModel | displayedBuilderIndexes = newDisplayedBuilderIndexes, selectedBuilderIndex = newSelectedBuilderIndex }
      in
        ( { model | builderTreeModel = newBuilderAppModel }, Cmd.none)

    SaveTab idx ->
      let
        markFileAsSaved : BuilderTree.Node -> BuilderTree.Node
        markFileAsSaved node =
          case node of
            BuilderTree.Folder f -> BuilderTree.Folder f
            BuilderTree.File f -> BuilderTree.File { f | isSaved = True }
        newBuilderTree = BuilderTree.modifyNode markFileAsSaved model.builderTreeModel.tree idx
        formerBuilderAppModel = model.builderTreeModel
        newBuilderAppModel = { formerBuilderAppModel | tree = newBuilderTree }
      in
        ({ model | builderTreeModel = newBuilderAppModel }, Cmd.none)

    BuilderMsg subMsg ->
      let
        mBuilder = Debug.log "mbuilder" (Maybe.andThen (BuilderTree.findBuilder model.builderTreeModel.tree) model.builderTreeModel.selectedBuilderIndex)
      in
        case (model.builderTreeModel.selectedBuilderIndex, mBuilder) of
          (Just idx, Just builder) ->
            let
              (updatedBuilder, cmdBuilder) = (Builder.update subMsg builder)
              action : BuilderTree.Node -> BuilderTree.Node
              action formerNode =
                case formerNode of
                  BuilderTree.Folder f -> BuilderTree.Folder f
                  BuilderTree.File f -> BuilderTree.File { f | builder = updatedBuilder }
              newBuilderTree = BuilderTree.modifyNode action model.builderTreeModel.tree idx
              formerBuilderAppModel = model.builderTreeModel
              newBuilderAppModel = { formerBuilderAppModel | tree = newBuilderTree }
            in
              ( { model | builderTreeModel = newBuilderAppModel }, Cmd.map BuilderMsg cmdBuilder)

          _ ->
            (model, Cmd.none)

    EnvSelectionMsg subMsg ->
      (model, Cmd.none)
