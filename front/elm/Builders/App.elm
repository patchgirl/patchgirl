module Builders.App exposing (..)

import Builders.Model exposing (..)
import Builders.Message exposing (..)

import BuilderTree.Model as BuilderTree
import BuilderTree.Util as BuilderTree
import Builder.App as Builder

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectTab idx ->
      ( { model | selectedBuilderIndex = Just idx }, Cmd.none)

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

        newDisplayedBuilderIndexes = List.filter (\x -> x /= idx) model.displayedBuilderIndexes
        newSelectedBuilderIndex =
          case Just idx == model.selectedBuilderIndex of
            False -> model.selectedBuilderIndex
            True -> findPrevious model.displayedBuilderIndexes idx
      in
        ( { model | displayedBuilderIndexes = newDisplayedBuilderIndexes,
          selectedBuilderIndex = newSelectedBuilderIndex }, Cmd.none)

    SaveTab idx ->
      let
        markFileAsSaved : BuilderTree.Node -> BuilderTree.Node
        markFileAsSaved node =
          case node of
            BuilderTree.Folder f -> BuilderTree.Folder f
            BuilderTree.File f -> BuilderTree.File { f | isSaved = True }
        newBuilderTree = BuilderTree.modifyNode markFileAsSaved model.tree idx
      in
        ({ model | tree = newBuilderTree }, Cmd.none)

    BuilderMsg subMsg ->
      let
        mBuilder = Debug.log "mbuilder" (Maybe.andThen (BuilderTree.findBuilder model.tree) model.selectedBuilderIndex)
      in
        case (model.selectedBuilderIndex, mBuilder) of
          (Just idx, Just builder) ->
            let
              (updatedBuilder, cmdBuilder) = (Builder.update subMsg builder)
              action : BuilderTree.Node -> BuilderTree.Node
              action formerNode =
                case formerNode of
                  BuilderTree.Folder f -> BuilderTree.Folder f
                  BuilderTree.File f -> BuilderTree.File { f | builder = updatedBuilder }
              newBuilderTree = BuilderTree.modifyNode action model.tree idx
            in
              ( { model | tree = newBuilderTree }, Cmd.map BuilderMsg cmdBuilder)

          _ ->
            (model, Cmd.none)
