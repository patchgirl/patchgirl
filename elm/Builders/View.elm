module Builders.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Builders.Message exposing (..)
import Builders.Model exposing (..)

import Builder.View as Builder
import Tree.Util as Tree
import Util.List as List

view : Model -> Html Msg
view model =
  let
    f idx = Tree.findBuilder model.tree idx
        |> Maybe.map Builder.view
        |> Maybe.map (Html.map BuilderMsg)
    builderAppView : List (Html Msg)
    builderAppView =
      List.flatten (List.map f model.displayedBuilderIndexes)
  in
    div [ id "builders" ] builderAppView
