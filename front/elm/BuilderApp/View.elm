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
import EnvironmentToRunSelection.App as EnvSelection

import Html.Events.Extra exposing (targetValueIntParse)
import Application.Type exposing (..)
import Json.Decode as Json
import Html.Events as Html
import Html as Html
import Html.Attributes as Html

view : Model a -> Element Msg
view model =
    let
        treeView : Element Msg
        treeView =
            column [ spacing 10 ]
                <| List.map (map TreeMsg) (BuilderTree.view model)

    in
        row [ width fill, paddingXY 10 0 ]
            [ column [ alignTop, spacing 20, centerX, paddingXY 20 0,  width (fillPortion 2) ]
                  [ el [ ] <| envSelectionView <| List.map .name model.environments
                  , el [ paddingXY 10 0 ] treeView
                  ]
            , el [ width (fillPortion 8) ] <| builderView model model.selectedBuilderIndex
            ]

envSelectionView : List String -> Element Msg
envSelectionView environmentNames =
    let
        entryView : Int -> String -> Html.Html Msg
        entryView idx envName =
            Html.option [ Html.value (String.fromInt idx) ] [ Html.text envName ]
    in
        html <|
            Html.div []
                [ Html.label [] [ Html.text "Env: " ]
                , Html.select [ Html.on "change" (Json.map EnvSelectionMsg targetValueIntParse) ]
                    (List.indexedMap entryView environmentNames)
                ]

builderView : Model a -> Maybe Int -> Element Msg
builderView model mIdx =
    let
        (RequestCollection _ requestNodes) = model.requestCollection
        mFile : Int -> Maybe BuilderApp.Model.File
        mFile idx = BuilderTree.findFile requestNodes idx
    in
        case Maybe.andThen mFile mIdx of
            Just file ->
                el [ width fill, height fill, spacing 20 ]
                    (map BuilderMsg (Builder.view file))

            Nothing ->
                el [ centerX ] (text "No request selected")
