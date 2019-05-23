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
            formerTree = model.tree
            newTree = { formerTree | selectedBuilderIndex = Just idx }
        in
            ( { model | tree = newTree }, Cmd.none)

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

        newDisplayedBuilderIndexes = List.filter (\x -> x /= idx) model.tree.displayedBuilderIndexes
        newSelectedBuilderIndex =
          case Just idx == model.tree.selectedBuilderIndex of
            False -> model.tree.selectedBuilderIndex
            True -> findPrevious model.tree.displayedBuilderIndexes idx
        formerTree = model.tree
        newTree = { formerTree | displayedBuilderIndexes = newDisplayedBuilderIndexes, selectedBuilderIndex = newSelectedBuilderIndex }
      in
        ( { model | tree = newTree }, Cmd.none)

    SaveTab idx ->
      let
        markFileAsSaved : BuilderTree.Node -> BuilderTree.Node
        markFileAsSaved node =
          case node of
            BuilderTree.Folder f -> BuilderTree.Folder f
            BuilderTree.File f -> BuilderTree.File { f | isSaved = True }
        newBuilderTree = BuilderTree.modifyNode markFileAsSaved model.tree.tree idx
        formerTree = model.tree
        newTree = { formerTree | tree = newBuilderTree }
      in
        ({ model | tree = newTree }, Cmd.none)

    BuilderMsg subMsg ->
      let
        mBuilder = Debug.log "mbuilder" (Maybe.andThen (BuilderTree.findBuilder model.tree.tree) model.tree.selectedBuilderIndex)
      in
        case (model.tree.selectedBuilderIndex, mBuilder) of
          (Just idx, Just builder) ->
            let
              (updatedBuilder, cmdBuilder) = (Builder.update subMsg builder)
              action : BuilderTree.Node -> BuilderTree.Node
              action formerNode =
                case formerNode of
                  BuilderTree.Folder f -> BuilderTree.Folder f
                  BuilderTree.File f -> BuilderTree.File { f | builder = updatedBuilder }
              newBuilderTree = BuilderTree.modifyNode action model.tree.tree idx
              formerTree = model.tree
              newTree = { formerTree | tree = newBuilderTree }
            in
              ( { model | tree = newTree }, Cmd.map BuilderMsg cmdBuilder)

          _ ->
            (model, Cmd.none)

    EnvSelectionMsg subMsg ->
      (model, Cmd.none)
