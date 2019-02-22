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
    builderTabsView = List.map tabView model.displayedBuilderIndexes
    builderAppsView = List.map (builderView model) model.displayedBuilderIndexes
  in
    div [ id "builders" ]
      [ div [ id "builderTabs" ] builderTabsView
      , div [] builderAppsView
      ]

tabView : Int -> Html Msg
tabView idx =
  div []
    [ a [ onClick (SelectTab idx) ] [ text "whatever" ]
    , a [ onClick (CloseTab idx) ] [ text "X" ]
    ]

builderView : Model -> Int -> Html Msg
builderView model idx =
  let
    mBuilder = Tree.findBuilder model.tree idx
  in
    case mBuilder of
      Just builder ->
        div [ hidden (not (model.selectedBuilderIndex == Just idx)) ]
            [ Html.map BuilderMsg (Builder.view builder) ]
      Nothing -> div [] []
