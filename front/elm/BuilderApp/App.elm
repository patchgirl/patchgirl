module BuilderApp.App exposing (..)

import BuilderApp.Model exposing (..)
import BuilderApp.Message exposing (..)

import BuilderApp.BuilderTree.Util as BuilderTree
import BuilderApp.Builder.App as Builder
import BuilderApp.Util exposing (..)
import Util.Maybe as Maybe

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DisplayBuilder idx ->
        let
            newModel = { model | selectedBuilderIndex = Just idx }
        in
            (newModel, Cmd.none)

    BuilderMsg subMsg ->
      let
        mBuilder = Maybe.andThen (BuilderTree.findBuilder model.tree) model.selectedBuilderIndex
      in
        case (model.selectedBuilderIndex, mBuilder) of
          (Just idx, Just builder) ->
            let
                (newBuilder, cmdBuilder) = (Builder.update subMsg builder)
                newBuilderTree = BuilderTree.modifyNode (changeFileBuilder newBuilder) model.tree idx
                newModel = { model | tree = newBuilderTree }
            in
                (newModel, Cmd.map BuilderMsg cmdBuilder)

          _ ->
            (model, Cmd.none)
