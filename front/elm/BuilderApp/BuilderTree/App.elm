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
        let
            (RequestCollection id requestNodes) = model.requestCollection
        in
            { model
                | requestCollection =
                  RequestCollection id (modifyRequestNode toggleFolder requestNodes idx)
            }

    Mkdir idx ->
        let
            (RequestCollection id requestNodes) = model.requestCollection
        in
            { model
                | requestCollection =
                  RequestCollection id (modifyRequestNode mkdir requestNodes idx)
            }

    Touch idx ->
        let
            (RequestCollection id requestNodes) = model.requestCollection
        in
            { model
                | requestCollection =
                  RequestCollection id (modifyRequestNode touch requestNodes idx)
            }

    ShowRenameInput idx ->
        let
            (RequestCollection id requestNodes) = model.requestCollection
        in
            { model
                | requestCollection =
                  RequestCollection id (modifyRequestNode displayRenameInput requestNodes idx)
            }

    Rename idx newName ->
        let
            (RequestCollection id requestNodes) = model.requestCollection
        in
            { model
                | requestCollection =
                  RequestCollection id (modifyRequestNode (rename newName) requestNodes idx)
            }

    ChangeName idx newName ->
        let
            (RequestCollection id requestNodes) = model.requestCollection
        in
            { model
                | requestCollection =
                  RequestCollection id (modifyRequestNode (tempRename newName) requestNodes idx)
            }

    Delete idx ->
        let
            (RequestCollection id requestNodes) = model.requestCollection
        in
            { model
                | requestCollection =
                    RequestCollection id (deleteRequestNode requestNodes idx)
            }

    DoNothing -> model
