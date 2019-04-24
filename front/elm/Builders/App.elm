module Builders.App exposing (..)

import Builders.Model exposing (..)
import Builders.Message exposing (..)

import Tree.Model as Tree
import Tree.Util as Tree
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
        markFileAsSaved : Tree.Node -> Tree.Node
        markFileAsSaved node =
          case node of
            Tree.Folder f -> Tree.Folder f
            Tree.File f -> Tree.File { f | isSaved = True }
        newTree = Tree.modifyNode markFileAsSaved model.tree idx
      in
        ({ model | tree = newTree }, Cmd.none)

    BuilderMsg subMsg ->
      let
        mBuilder = Debug.log "mbuilder" (Maybe.andThen (Tree.findBuilder model.tree) model.selectedBuilderIndex)
      in
        case (model.selectedBuilderIndex, mBuilder) of
          (Just idx, Just builder) ->
            let
              (updatedBuilder, cmdBuilder) = (Builder.update subMsg builder)
              action : Tree.Node -> Tree.Node
              action formerNode =
                case formerNode of
                  Tree.Folder f -> Tree.Folder f
                  Tree.File f -> Tree.File { f | builder = updatedBuilder }
              newTree = Tree.modifyNode action model.tree idx
            in
              ( { model | tree = newTree }, Cmd.map BuilderMsg cmdBuilder)

          _ ->
            (model, Cmd.none)
