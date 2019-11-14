module BuilderApp.View exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events

import BuilderApp.Message exposing (..)
import BuilderApp.Model exposing (..)

import BuilderApp.BuilderTree.Util as BuilderTree
import Util.List as List
import Util.Maybe as Maybe
import BuilderApp.Builder.View as Builder
import BuilderApp.Builder.Model as Builder
import BuilderApp.BuilderTree.View as BuilderTree

import Application.Type exposing (..)

view : Model a -> Element Msg
view model =
    row [ width fill ]
      [ column [ paddingXY 10 0, spacing 10, width (px 200) ] <|
            List.map (map TreeMsg) (BuilderTree.view model)
      , builderView model model.selectedBuilderIndex
      ]

builderView : Model a -> Maybe Int -> Element Msg
builderView model mIdx =
    let
        (RequestCollection _ requestNodes) = model.requestCollection
        mFile : Int -> Maybe BuilderApp.Model.File
        mFile idx = BuilderTree.findFile requestNodes idx
        title file =
            case file.isSaved of
                True -> notEditedValue file.name
                False -> (notEditedValue file.name) ++ " *"
    in
        case Maybe.andThen mFile mIdx of
            Just file ->
                column [ width fill, height fill, spacing 20 ]
                    [ el [ centerX ] <| text (title file)
                    , map BuilderMsg (Builder.view file)
                    ]
            Nothing ->
                el [ centerX ] (text "No request selected")
