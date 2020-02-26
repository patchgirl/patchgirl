module BuilderApp.App exposing (..)

import Uuid

import BuilderApp.BuilderTree.App as BuilderTree
import BuilderApp.Builder.App as Builder
import Util.Maybe as Maybe
import Api.Generated as Client
import Api.Converter as Client
import Http as Http
import Application.Type exposing (..)
import EnvironmentToRunSelection.App as EnvSelection
import List.Extra as List
import InitializedApplication.Model as InitializedApplication
import VarApp.Model as VarApp

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events

import Html.Events as Html
import Html as Html
import Html.Attributes as Html
import ViewUtil exposing (..)

import Json.Decode as Json
import Html.Events.Extra exposing (targetValueIntParse)


-- * model


type alias Model a =
    { a
        | selectedBuilderId : Maybe Uuid.Uuid
        , displayedBuilderIndex : Maybe Int
        , requestCollection : RequestCollection
        , displayedRequestNodeMenuId : Maybe Uuid.Uuid
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , varAppModel : VarApp.Model
    }


-- * message


type Msg
  = BuilderMsg Builder.Msg
  | TreeMsg BuilderTree.Msg
  | EnvSelectionMsg Int


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
        EnvSelectionMsg idx ->
            let
                newModel =
                    { model | selectedEnvironmentToRunIndex = Just idx }
            in
                (newModel, Cmd.none)

        TreeMsg subMsg ->
            let
                (newModel, newSubMsg) = BuilderTree.update subMsg model
            in
                (newModel, Cmd.map TreeMsg newSubMsg)

        BuilderMsg subMsg ->
            let
                (RequestCollection requestCollectionId requestNodes) = model.requestCollection
                mFile : Maybe File
                mFile = Maybe.andThen (BuilderTree.findFile requestNodes) model.selectedBuilderId
            in
                case (model.selectedBuilderId, mFile) of
                    (Just id, Just file) ->
                        let
                            builderModel =
                                (InitializedApplication.getEnvironmentKeyValuesToRun model)

                            (newFile, newSubMsg) =
                                Builder.update subMsg builderModel model.varAppModel.vars file

                            newBuilderTree =
                                List.map (BuilderTree.modifyRequestNode2 id (changeFileBuilder newFile)) requestNodes

                            newModel =
                                { model
                                    | requestCollection = RequestCollection requestCollectionId newBuilderTree
                                }
                        in
                            (newModel, Cmd.map BuilderMsg newSubMsg)

                    _ ->
                        (model, Cmd.none)


-- * util


changeFileBuilder : File -> RequestNode -> RequestNode
changeFileBuilder newFile node =
    case node of
        RequestFolder f ->
            RequestFolder f
        RequestFile f ->
            RequestFile newFile


-- * view

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
        mFile : Uuid.Uuid -> Maybe File
        mFile idx = BuilderTree.findFile requestNodes idx
    in
        case Maybe.andThen mFile mId of
            Just file ->
                el [ width fill, height fill, spacing 20 ]
                    (map BuilderMsg (Builder.view file))

            Nothing ->
                el [ centerX ] (text "No request selected")
