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
        { model | requestCollection = (modifyRequestNode toggleFolder model.requestCollection idx) }

    Mkdir idx ->
        { model | requestCollection = (modifyRequestNode mkdir model.requestCollection idx) }

    Touch idx ->
        { model | requestCollection = (modifyRequestNode touch model.requestCollection idx) }

    ShowRenameInput idx ->
        { model | requestCollection = (modifyRequestNode displayRenameInput model.requestCollection idx) }

    Rename idx newName ->
        { model | requestCollection = (modifyRequestNode (rename newName) model.requestCollection idx) }

    Delete idx ->
        { model | requestCollection = (deleteRequestNode model.requestCollection idx) }
