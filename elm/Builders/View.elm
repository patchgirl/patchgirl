module Builders.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Builders.Message exposing (..)
import Builders.Model exposing (..)

import Builder.View as Builder
import Tree.Util as Tree
import Util.List as List
import Util.Maybe as Maybe
import Builder.Model as Builder

view : Model -> Html Msg
view model =
  let
    builderAppView = List.map (tabView model) model.displayedBuilderIndexes
  in
    div [ id "builders" ] builderAppView

tabView : Model -> Int -> Html Msg
tabView model idx =
  let
    mBuilder = Tree.findBuilder model.tree idx
  in
    case mBuilder of
      Just builder ->
        div []
          [ div [ onClick (SelectTab idx) ] [ text "whatever" ]
          , div [ hidden (not (model.selectedBuilderIndex == Just idx)) ]
            [ Html.map BuilderMsg (Builder.view builder)
            ]
          ]
      Nothing -> div [] []
