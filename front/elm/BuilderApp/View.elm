module BuilderApp.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import BuilderApp.Message exposing (..)
import BuilderApp.Model exposing (..)

import BuilderApp.BuilderTree.Util as BuilderTree
import BuilderApp.BuilderTree.Model as BuilderTree
import Util.List as List
import Util.Maybe as Maybe
import BuilderApp.Builder.View as Builder
import BuilderApp.Builder.Model as Builder

view : Model -> Html Msg
view model =
    let
        -- TODO rename
        builderAppsView2 = builderView2 model model.builderTreeModel.displayedBuilderIndex
    in
        div [ id "builderPanel" ]
            [ div [] [ builderAppsView2 ]
            ]

builderView2 : Model -> Maybe Int -> Html Msg
builderView2 model mIdx =
  let
    mBuilder : Int -> Maybe Builder.Model
    mBuilder idx = BuilderTree.findBuilder model.builderTreeModel.tree idx
  in
    case Maybe.andThen mBuilder mIdx of
      Just builder ->
        div []
            [ Html.map BuilderMsg (Builder.view builder) ]
      Nothing -> div [] []


-- TODO remove
tabView : Model -> Maybe Int -> Int -> Html Msg
tabView model mSelectedIdx idx =
  let
    activeClass =
      case mSelectedIdx == Just idx of
        True -> "isActive"
        False -> ""
    savedView = a [ href "#" ] [ ]
    unsavedView file = a [ href "#", onClick (SaveTab idx) ] [ text "*" ]
    savingView file =
      case file.isSaved of
        True -> savedView
        False -> unsavedView file
  in
    case BuilderTree.findNode model.builderTreeModel.tree idx of
      Just (BuilderTree.File file)  ->
        li [ class activeClass ]
          [ a [ href "#", onClick (SelectTab idx) ] [ text (file.name) ]
          , savingView file
          , a [ href "#", onClick (CloseTab idx), class "closeBuilder" ] [ span [ class "fas fa-times" ] [] ]
          ]
      _ -> li [] []

-- TODO remove
builderView : Model -> Int -> Html Msg
builderView model idx =
  let
    mBuilder = BuilderTree.findBuilder model.builderTreeModel.tree idx
  in
    case mBuilder of
      Just builder ->
        div [ hidden (not (model.builderTreeModel.selectedBuilderIndex == Just idx)) ]
            [ Html.map BuilderMsg (Builder.view builder) ]
      Nothing -> div [] []
