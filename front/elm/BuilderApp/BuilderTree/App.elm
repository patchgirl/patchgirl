module BuilderApp.BuilderTree.App exposing (..)

import BuilderApp.Model exposing (..)
import BuilderApp.BuilderTree.Message exposing (..)
import BuilderApp.BuilderTree.Util exposing (..)

import BuilderApp.Builder.App as Builder
import BuilderApp.Builder.Model as Builder

import Util.Maybe as Maybe

update : Msg -> Model a -> Model a
update msg model =
  case msg of
    SetDisplayedBuilder idx ->
        { model | selectedBuilderIndex = Just idx }

    ToggleMenu idx ->
      let
        newDisplayedNodeMenuIndex =
          case Maybe.exists model.displayedNodeMenuIndex ((==) idx) of
            True -> Nothing
            False -> Just idx
      in
        { model | displayedNodeMenuIndex = newDisplayedNodeMenuIndex }

    ToggleFolder idx ->
        { model | tree = (modifyNode toggleFolder model.tree idx) }

    Mkdir idx ->
        { model | tree = (modifyNode mkdir model.tree idx) }

    Touch idx ->
        { model | tree = (modifyNode touch model.tree idx) }

    ShowRenameInput idx ->
        { model | tree = (modifyNode displayRenameInput model.tree idx) }

    Rename idx newName ->
        { model | tree = (modifyNode (rename newName) model.tree idx) }

    Delete idx ->
        { model | tree = (deleteNode model.tree idx) }
