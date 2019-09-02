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
    div [ id "builderPanel" ]
        [ div [] [ builderView model model.displayedBuilderIndex ]
        ]

builderView : Model -> Maybe Int -> Html Msg
builderView model mIdx =
  let
    mBuilder : Int -> Maybe Builder.Model
    mBuilder idx = BuilderTree.findBuilder model.tree idx
  in
    case Maybe.andThen mBuilder mIdx of
      Just builder ->
        div []
            [ Html.map BuilderMsg (Builder.view builder) ]
      Nothing -> div [] []
