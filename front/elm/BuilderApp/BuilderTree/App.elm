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
        newDisplayedRequestNodeMenuIndex =
          case Maybe.exists model.displayedRequestNodeMenuIndex ((==) idx) of
            True -> Nothing
            False -> Just idx
      in
        { model | displayedRequestNodeMenuIndex = newDisplayedRequestNodeMenuIndex }

    ToggleFolder idx ->
        { model | tree = (modifyRequestNode toggleFolder model.tree idx) }

    Mkdir idx ->
        { model | tree = (modifyRequestNode mkdir model.tree idx) }

    Touch idx ->
        { model | tree = (modifyRequestNode touch model.tree idx) }

    ShowRenameInput idx ->
        { model | tree = (modifyRequestNode displayRenameInput model.tree idx) }

    Rename idx newName ->
        { model | tree = (modifyRequestNode (rename newName) model.tree idx) }

    Delete idx ->
        { model | tree = (deleteRequestNode model.tree idx) }
