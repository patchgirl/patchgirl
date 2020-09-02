module ScenarioBuilderApp.App exposing (..)

import Application.Model as Application
import Application.Type exposing (..)
import Element exposing (..)
import Element.Font as Font
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
import Uuid exposing(Uuid)
import Browser.Navigation as Navigation


-- * model


type alias Model a =
    { a
        | notification : Maybe Notification
        , session : Session
        , whichModal : Maybe Modal
        , scenarioCollection : ScenarioCollection
        , requestCollection : RequestCollection
        , pgCollection : PgCollection
        , displayedScenarioNodeMenuId : Maybe Uuid
        , displayedScenarioId : Maybe Uuid
        , displayedSceneId : Maybe Uuid
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , runnerRunning : Bool
        , navigationKey : Navigation.Key
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


getBuilder : Model a -> Maybe ScenarioBuilder.Model
getBuilder model =
    let
        (ScenarioCollection scenarioCollectionId scenarioNodes) =
            model.scenarioCollection

        mFile : Maybe ScenarioFileRecord
        mFile =
            Maybe.andThen (ScenarioTree.findFile scenarioNodes) (model.displayedScenarioId)
    in
    case mFile of
        Just file ->
            let
                keyValuesToRun =
                    Application.getEnvironmentKeyValuesToRun model
            in
            Just (convertFromFileToBuilder file model scenarioCollectionId keyValuesToRun)

        _ ->
            Nothing

convertFromFileToBuilder :
    ScenarioFileRecord
    -> Model a
    -> Uuid
    -> List (Storable NewKeyValue KeyValue)
    -> ScenarioBuilder.Model
convertFromFileToBuilder file model scenarioCollectionId keyValuesToRun =
    { notification = model.notification
    , session = model.session
    , whichModal = model.whichModal
    , id = file.id
    , scenarioCollectionId = scenarioCollectionId
    , requestCollection = model.requestCollection
    , pgCollection = model.pgCollection
    , scenes = file.scenes
    , keyValues = keyValuesToRun
    , name = file.name
    , displayedSceneId = model.displayedSceneId
    , whichResponseView = file.whichResponseView
    , environments = model.environments
    , environmentId = file.environmentId
    , runnerRunning = model.runnerRunning
    , navigationKey = model.navigationKey
    }

convertFromBuilderToFile : ScenarioBuilder.Model -> ScenarioFileRecord
convertFromBuilderToFile builder =
    { id = builder.id
    , name = builder.name
    , scenes = builder.scenes
    , whichResponseView = builder.whichResponseView
    , environmentId = builder.environmentId
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
    let
        enableRunnerBanner =
            row [ width fill
              , paddingXY 0 10
              , centerY, centerX
              , Background.color secondaryColor
              , Font.color primaryColor
              , Font.center
              ]
              [ el [ centerX ] (text "Offline mode - Run the ")
              , link [ centerX ] { label = el [ Font.underline ] (text "Patchgirl Runner App")
                                 , url = href (DocumentationPage PatchGirlRunnerAppDoc)
                                 }
              , el [ centerX ] (text " to enable scenarios!")
              ]
    in
    column [ width fill, spacing 10 ]
        [ if not model.runnerRunning then enableRunnerBanner else none
        , wrappedRow
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
        ]

builderView : Model a -> Element Msg
builderView model =
    case getBuilder model of
        Just builder ->
            el [ width (fillPortion 9), centerX, alignTop ]
                (map ScenarioBuilderMsg (ScenarioBuilder.view builder))

        Nothing ->
            el [ width (fillPortion 9), centerX, alignTop ]
                <| el [ centerX
                      , alignTop
                      , padding 20
                      , Background.color white
                      , boxShadow
                      ] (text "No scenario selected")
