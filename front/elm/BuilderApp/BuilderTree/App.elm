module BuilderApp.BuilderTree.App exposing (..)

import BuilderApp.BuilderTree.Model exposing (..)
import BuilderApp.BuilderTree.Message exposing (..)
import BuilderApp.BuilderTree.Util exposing (..)

import BuilderApp.Builder.App as Builder
import BuilderApp.Builder.Model as Builder

import Util.Maybe as Maybe

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetDisplayedBuilder idx ->
        ({ model | selectedBuilderIndex = Just idx }
        , Cmd.none)

    ToggleMenu idx ->
      let
        newDisplayedNodeMenuIndex =
          case Maybe.exists model.displayedNodeMenuIndex ((==) idx) of
            True -> Nothing
            False -> Just idx
      in
        ( { model | displayedNodeMenuIndex = newDisplayedNodeMenuIndex }
        , Cmd.none)

    ToggleFolder idx ->
        ( { model | tree = (modifyNode toggleFolder model.tree idx) }
        , Cmd.none)

    Mkdir idx ->
        ( { model | tree = (modifyNode mkdir model.tree idx) }
        , Cmd.none)

    Touch idx ->
        ( { model | tree = (modifyNode touch model.tree idx) }
        , Cmd.none)

    ShowRenameInput idx ->
        ( { model | tree = (modifyNode displayRenameInput model.tree idx) }
        , Cmd.none)

    Rename idx newName ->
        ( { model | tree = (modifyNode (rename newName) model.tree idx) }
        , Cmd.none)

    Delete idx ->
      ( { model | tree = (deleteNode model.tree idx) }, Cmd.none)
