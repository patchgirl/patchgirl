module BuilderApp.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import BuilderApp.Message exposing (..)
import BuilderApp.Model exposing (..)

import BuilderApp.BuilderTree.Util as BuilderTree
import Util.List as List
import Util.Maybe as Maybe
import BuilderApp.Builder.View as Builder
import BuilderApp.Builder.Model as Builder
import BuilderApp.BuilderTree.View as BuilderTree

view : Model2 a -> Html Msg
view model =
    div [ id "builderApp" ]
      [ div [ id "treeView" ] [ Html.map TreeMsg (BuilderTree.view model) ]
      , div [ id "builderPanel" ]
          [ div [] [ builderView model model.selectedBuilderIndex ]
          ]
      ]

builderView : Model2 a -> Maybe Int -> Html Msg
builderView model mIdx =
  let
    mBuilder : Int -> Maybe Builder.Model
    mBuilder idx = BuilderTree.findBuilder model.tree <| Debug.log "builder" idx
  in
    case Maybe.andThen mBuilder mIdx of
      Just builder ->
        div []
            [ Html.map BuilderMsg (Builder.view builder) ]
      Nothing -> div [] []
