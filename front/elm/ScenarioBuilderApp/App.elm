module ScenarioBuilderApp.App exposing (..)

import Application.Model as Application
import Application.Type exposing (..)
import Element exposing (..)
import Element.Background as Background
import Html as Html
import Html.Attributes as Html
import Html.Events as Html
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Json
import Modal exposing (Modal(..))
import Page exposing (..)
import ScenarioBuilderApp.ScenarioBuilder.App as ScenarioBuilder
import ScenarioBuilderApp.ScenarioTree.App as ScenarioTree
import Util exposing (..)
import Uuid



-- * model


type alias Model a =
    { a
        | notification : Maybe String
        , session : Session
        , whichModal : Maybe Modal
        , scenarioCollection : ScenarioCollection
        , requestCollection : RequestCollection
        , displayedScenarioNodeMenuId : Maybe Uuid.Uuid
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , page : Page
    }



-- * message


type Msg
    = ScenarioBuilderMsg ScenarioBuilder.Msg
    | ScenarioTreeMsg ScenarioTree.Msg



-- * update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        ScenarioTreeMsg subMsg ->
            let
                ( newModel, newSubMsg ) =
                    ScenarioTree.update subMsg model
            in
            ( newModel, Cmd.map ScenarioTreeMsg newSubMsg )

        ScenarioBuilderMsg subMsg ->
            case getBuilder model of
                Just builder ->
                    let
                        (ScenarioCollection scenarioCollectionId scenarioNodes) =
                            model.scenarioCollection

                        ( newBuilder, newSubMsg ) =
                            ScenarioBuilder.update subMsg builder

                        newBuilderTree =
                            List.map (ScenarioTree.modifyScenarioNode builder.id (changeFileBuilder newBuilder)) scenarioNodes

                        newModel =
                            { model
                                | scenarioCollection = ScenarioCollection scenarioCollectionId newBuilderTree
                                , notification = newBuilder.notification
                                , whichModal = newBuilder.whichModal
                            }

                        newMsg =
                            Cmd.map ScenarioBuilderMsg newSubMsg
                    in
                    ( newModel, newMsg )

                _ ->
                    ( model, Cmd.none )



-- * util


getSelectedBuilderId : Model a -> Maybe Uuid.Uuid
getSelectedBuilderId model =
    case model.page of
        ScenarioPage (Just id) ->
            Just id

        _ ->
            Nothing


getBuilder : Model a -> Maybe ScenarioBuilder.Model
getBuilder model =
    let
        (ScenarioCollection scenarioCollectionId scenarioNodes) =
            model.scenarioCollection

        mFile : Maybe ScenarioFileRecord
        mFile =
            Maybe.andThen (ScenarioTree.findFile scenarioNodes) (getSelectedBuilderId model)
    in
    case mFile of
        Just file ->
            let
                keyValuesToRun =
                    Application.getEnvironmentKeyValuesToRun model
            in
            Just (convertFromFileToBuilder file scenarioCollectionId model.session model.requestCollection keyValuesToRun model.notification model.whichModal)

        _ ->
            Nothing


convertFromFileToBuilder :
    ScenarioFileRecord
    -> Uuid.Uuid
    -> Session
    -> RequestCollection
    -> List (Storable NewKeyValue KeyValue)
    -> Maybe String
    -> Maybe Modal
    -> ScenarioBuilder.Model
convertFromFileToBuilder file scenarioCollectionId session requestCollection keyValuesToRun notification whichModal =
    { notification = notification
    , session = session
    , whichModal = whichModal
    , id = file.id
    , scenarioCollectionId = scenarioCollectionId
    , requestCollection = requestCollection
    , scenes = file.scenes
    , keyValues = keyValuesToRun
    , name = file.name
    , showDetailedSceneView = file.showDetailedSceneView
    , whichResponseView = file.whichResponseView
    }


convertFromBuilderToFile : ScenarioBuilder.Model -> ScenarioFileRecord
convertFromBuilderToFile builder =
    { id = builder.id
    , name = builder.name
    , scenes = builder.scenes
    , showDetailedSceneView = builder.showDetailedSceneView
    , whichResponseView = builder.whichResponseView
    }


changeFileBuilder : ScenarioBuilder.Model -> ScenarioNode -> ScenarioNode
changeFileBuilder builder node =
    case node of
        ScenarioFolder f ->
            ScenarioFolder f

        ScenarioFile f ->
            ScenarioFile (convertFromBuilderToFile builder)



-- * view


view : Model a -> Element Msg
view model =
    wrappedRow
        [ width fill
        , paddingXY 10 0
        , spacing 10
        ]
        [ el
            [ alignTop
            , spacing 20
            , centerX
            , padding 20
            , width (fillPortion 1)
            , Background.color white
            , boxShadow
            ] <| el [ paddingXY 10 0 ] (map ScenarioTreeMsg (ScenarioTree.view model))

        , builderView model
        ]

builderView : Model a -> Element Msg
builderView model =
    case getBuilder model of
        Just builder ->
            el [ width (fillPortion 9), centerX, alignTop ]
                (map ScenarioBuilderMsg (ScenarioBuilder.view builder))

        Nothing ->
            el [ width (fillPortion 9)
               , centerX, centerY, alignTop
               ]
                <| el ( [ centerX
                        , alignTop
                        , padding 20
                        , spacing 10
                        ] ++ boxAttrs
                      ) (text "No scenario selected")
