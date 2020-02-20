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
import BuilderApp.Builder.App as Builder
import BuilderApp.BuilderTree.View as BuilderTree
import EnvironmentToRunSelection.App as EnvSelection

import Html.Events.Extra exposing (targetValueIntParse)
import Application.Type exposing (..)
import Json.Decode as Json
import Html.Events as Html
import Html as Html
import Html.Attributes as Html
import Uuid
import ViewUtil exposing (..)

view : Model a -> Element Msg
view model =
        wrappedRow [ width fill
                   , paddingXY 10 0
                   , spacing 10
                   ]
            [ column [ alignTop
                     , spacing 20
                     , centerX
                     , padding 20
                     , width (fillPortion 1)
                     , Background.color white
                     , boxShadow
                     ]
                  [ el [ ] <| envSelectionView <| List.map .name model.environments
                  , el [ paddingXY 10 0 ] (map TreeMsg (BuilderTree.view model))
                  ]
            , el [ width (fillPortion 9) ]
                <| builderView model model.selectedBuilderId
            ]

envSelectionView : List (Editable String) -> Element Msg
envSelectionView environmentNames =
    let
        entryView : Int -> Editable String -> Html.Html Msg
        entryView idx envName =
            Html.option [ Html.value (String.fromInt idx) ] [ Html.text (editedOrNotEditedValue envName) ]
    in
        html <|
            Html.div []
                [ Html.label [] [ Html.text "Env: " ]
                , Html.select [ Html.on "change" (Json.map EnvSelectionMsg targetValueIntParse) ]
                    (List.indexedMap entryView environmentNames)
                ]

builderView : Model a -> Maybe Uuid.Uuid -> Element Msg
builderView model mId =
    let
        (RequestCollection _ requestNodes) = model.requestCollection
        mFile : Uuid.Uuid -> Maybe BuilderApp.Model.File
        mFile idx = BuilderTree.findFile requestNodes idx
    in
        case Maybe.andThen mFile mId of
            Just file ->
                el [ width fill, height fill, spacing 20 ]
                    (map BuilderMsg (Builder.view file))

            Nothing ->
                el [ centerX ] (text "No request selected")
