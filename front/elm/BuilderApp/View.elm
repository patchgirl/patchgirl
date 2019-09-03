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

view : Model a -> Html Msg
view model =
    div [ id "builderApp" ]
      [ div [ id "treeView" ] [ Html.map TreeMsg (BuilderTree.view model) ]
      , div [ id "builderPanel" ]
          [ div [] [ builderView model model.selectedBuilderIndex ]
          ]
      ]

builderView : Model a -> Maybe Int -> Html Msg
builderView model mIdx =
  let
    mFile : Int -> Maybe BuilderApp.Model.File2
    mFile idx = BuilderTree.findFile model.tree idx
    title file =
        case file.isSaved of
            True -> file.name
            False -> file.name ++ " *"
  in
    case Maybe.andThen mFile mIdx of
      Just file ->
        div []
            [ h1 [] [ text (title file) ]
            , Html.map BuilderMsg (Builder.view file.builder)
            ]
      Nothing -> div [] []
