module ScenarioBuilderApp.ScenarioBuilder.App exposing (..)

import Api.Converter as Client
import Api.Generated as Client
import Dict
import Application.Type exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Http
import List.Extra as List
import Modal exposing (Modal(..))
import Random
import RequestBuilderApp.RequestTree.Util as RequestTree
import RequestComputation exposing (..)
import Util exposing (..)
import Uuid exposing (Uuid)
import RequestComputation
import RequestBuilderApp.RequestBuilder.ResponseView exposing(..)
import Page exposing(..)


-- * model


type alias Model =
    { session : Session
    , notification : Maybe String
    , whichModal : Maybe Modal
    , id : Uuid.Uuid
    , requestCollection : RequestCollection
    , scenarioCollectionId : Uuid
    , scenes : List Scene
    , keyValues : List (Storable NewKeyValue KeyValue)
    , name : Editable String
    , showDetailedSceneView : Maybe Uuid
    , whichResponseView : HttpResponseView
    }



-- * message


type
    Msg
    -- create scene
    = ShowHttpRequestSelectionModal (Maybe Uuid)
    | GenerateRandomUUIDForScene (Maybe Uuid) Uuid
    | SelectRequestFile (Maybe Uuid) Uuid Uuid
    | AskCreateScene (Maybe Uuid) Uuid Uuid
    | CloseModal
      -- delete scene
    | AskDeleteScene Uuid
    | DeleteScene Uuid
    | ServerError
      -- scenario
    | AskRunScenario
    | ScenarioProcessed ScenarioComputationOutput
      -- detailed view
    | ShowDetailedView Uuid
    | HideDetailedView
    | ShowBodyResponseView
    | ShowHeaderResponseView
      -- other
    | DoNothing


-- * update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of


-- ** create scene


        ShowHttpRequestSelectionModal sceneParentId ->
            let
                newModel =
                    { model | whichModal = Just (SelectHttpRequestModal sceneParentId) }
            in
            ( newModel, Cmd.none )

        GenerateRandomUUIDForScene sceneParentId requestFileNodeId ->
            let
                newMsg =
                    Random.generate (AskCreateScene sceneParentId requestFileNodeId) Uuid.uuidGenerator
            in
            ( model, newMsg )

        AskCreateScene sceneParentId requestFileNodeId newSceneId ->
            let
                payload =
                    { newSceneId = newSceneId
                    , newSceneSceneNodeParentId = sceneParentId
                    , newSceneRequestFileNodeId = requestFileNodeId
                    }

                newMsg =
                    Client.postApiScenarioNodeByScenarioNodeIdScene "" (getCsrfToken model.session) model.id payload (createSceneResultToMsg sceneParentId requestFileNodeId newSceneId)
            in
            ( model, newMsg )

        SelectRequestFile sceneParentId requestFileNodeId newSceneId ->
            let
                newScene =
                    mkDefaultScene newSceneId requestFileNodeId

                newScenes =
                    case sceneParentId of
                        Nothing ->
                            model.scenes ++ [ newScene ]

                        Just parentId ->
                            addToListAfterPredicate model.scenes (\scene -> scene.id == parentId) newScene

                newModel =
                    { model
                        | whichModal = Nothing
                        , scenes = newScenes
                    }
            in
            ( newModel, Cmd.none )

        -- ** delete scene
        AskDeleteScene sceneId ->
            let
                newMsg =
                    Client.deleteApiScenarioNodeByScenarioNodeIdSceneBySceneId "" (getCsrfToken model.session) model.id sceneId (deleteSceneResultToMsg sceneId)
            in
            ( model, newMsg )

        DeleteScene id ->
            let
                newScenes =
                    List.filter (not << (\scene -> scene.id == id)) model.scenes

                newModel =
                    { model | scenes = newScenes }
            in
            ( newModel, Cmd.none )


-- ** scenario


        AskRunScenario ->
            let
                sceneToInputScene : Scene -> Client.InputScene
                sceneToInputScene scene =
                    let
                        requestComputationInput =
                            Maybe.map (buildRequestComputationInput model.keyValues) (findFileRecord model scene.requestFileNodeId)
                    in
                    { inputSceneId = scene.id
                    , inputSceneRequestFileNodeId = scene.requestFileNodeId
                    , inputSceneRequestComputationInput =
                        Maybe.map Client.convertRequestComputationInputFromFrontToBack requestComputationInput
                    }

                payload =
                    { inputScenarioId = model.id
                    , inputScenarioScenes = List.map sceneToInputScene model.scenes
                    }

                newMsg =
                    Client.postApiScenarioComputation "" (getCsrfToken model.session) payload runScenarioResultToMsg
            in
            ( model, newMsg )

        ScenarioProcessed scenarioComputationOutput ->
            let
                mergeSceneComputationOutputResult : Scene -> Scene
                mergeSceneComputationOutputResult scene =
                    let
                        sceneComputation =
                            List.find (\s -> s.sceneId == scene.id) scenarioComputationOutput.scenes
                                |> Maybe.map .requestComputationOutput
                    in
                    { scene | computationOutput = sceneComputation }

                newScenes =
                    List.map mergeSceneComputationOutputResult model.scenes

                newModel =
                    { model | scenes = newScenes }
            in
            ( newModel, Cmd.none )


-- ** detailed view


        ShowDetailedView id ->
            let
                newModel =
                    { model | showDetailedSceneView = Just id }
            in
            (newModel, Cmd.none)

        HideDetailedView ->
            let
                newModel =
                    { model | showDetailedSceneView = Nothing }
            in
            (newModel, Cmd.none)

        ShowBodyResponseView ->
            let
                newModel =
                    { model | whichResponseView = BodyResponseView }
            in
            ( newModel, Cmd.none )

        ShowHeaderResponseView ->
            let
                newModel =
                    { model | whichResponseView = HeaderResponseView }
            in
            ( newModel, Cmd.none )


-- ** other


        CloseModal ->
            let
                newModel =
                    { model | whichModal = Nothing }
            in
            ( newModel, Cmd.none )

        ServerError ->
            ( model, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )


-- * util


mkDefaultScene : Uuid.Uuid -> Uuid.Uuid -> Scene
mkDefaultScene id requestFileNodeId =
    { id = id
    , requestFileNodeId = requestFileNodeId
    , computationOutput = Nothing
    }


runScenarioResultToMsg : Result Http.Error Client.ScenarioComputationOutput -> Msg
runScenarioResultToMsg result =
    case result of
        Ok scenarioComputationOutput ->
            ScenarioProcessed (Client.convertScenarioComputationOutputFromBackToFront scenarioComputationOutput)

        Err error ->
            Debug.todo "server error" ServerError


deleteSceneResultToMsg : Uuid.Uuid -> Result Http.Error () -> Msg
deleteSceneResultToMsg sceneId result =
    case result of
        Ok () ->
            DeleteScene sceneId

        Err error ->
            Debug.todo "server error" ServerError


createSceneResultToMsg : Maybe Uuid -> Uuid -> Uuid -> Result Http.Error () -> Msg
createSceneResultToMsg sceneParentId requestFileNodeId newSceneId result =
    case result of
        Ok () ->
            SelectRequestFile sceneParentId requestFileNodeId newSceneId

        Err error ->
            Debug.todo "server error" ServerError


findFileRecord : Model -> Uuid -> Maybe RequestFileRecord
findFileRecord model id =
    let
        (RequestCollection _ requestNodes) =
            model.requestCollection
    in
    RequestTree.findFile requestNodes id


-- * view


view : Model -> Element Msg
view model =
    let
        scenesView =
            case model.scenes of
                [] ->
                    addNewSceneView

                scenes ->
                    column [ centerX, spacing 10 ] (List.map (sceneView model) scenes)

        scenarioSettingView =
            column [ width fill, centerX ]
                [ Input.button [ centerX ]
                    { onPress = Just AskRunScenario
                    , label =
                        row [ centerX, centerY ]
                            [ iconWithTextAndColorAndAttr "send" "Run" primaryColor []
                            ]
                    }
                ]

        sceneAndFileRecordDetailToShow : Maybe (Scene, RequestFileRecord)
        sceneAndFileRecordDetailToShow =
            let
                mScene =
                    Maybe.andThen (\sceneToShowId ->
                                   List.find (\scene -> scene.id == sceneToShowId) model.scenes) model.showDetailedSceneView

                mFileRecord =
                    Maybe.andThen (\scene -> findFileRecord model scene.requestFileNodeId) mScene

            in
            case (mScene, mFileRecord) of
                (Just scene, Just fileRecord) ->
                    Just (scene, fileRecord)

                _ ->
                    Nothing

    in
    case sceneAndFileRecordDetailToShow of
        Nothing ->
            wrappedRow [ width fill, centerX ]
                [ el [ width (fillPortion 8) ] scenesView
                , el [ width (fillPortion 2) ] scenarioSettingView
                ]

        Just (scene, requestFileRecord) ->
            wrappedRow [ height fill, width fill ]
                [ el [ width (fillPortion 4), height fill ] scenesView
                , el [ width (fillPortion 1), height fill ] scenarioSettingView
                , el [ width (fillPortion 5), height fill ] (detailedSceneView model scene requestFileRecord)
                ]



-- ** add new scene view


addNewSceneView : Element Msg
addNewSceneView =
    el [ width fill, centerX ]
        (Input.button [ centerX ]
            { onPress = Just (ShowHttpRequestSelectionModal Nothing)
            , label =
                row [ centerX, centerY ]
                    [ addIcon
                    , el [] (text "Add a scene to your scenario")
                    ]
            }
        )



-- ** scene view


sceneView : Model -> Scene -> Element Msg
sceneView model { id, requestFileNodeId, computationOutput } =
    let
        (RequestCollection _ requestNodes) =
            model.requestCollection

        mRequestFileRecord =
            RequestTree.findFile requestNodes requestFileNodeId

        sceneComputationAttrs =
            case computationOutput of
                Nothing ->
                    [ Border.color white ]

                Just SceneNotRun ->
                    [ borderError, backgroundError ]

                Just (SceneRun _) ->
                    [ borderSuccess, backgroundSuccess ]
    in
    case mRequestFileRecord of
        Just { name } ->
            column [ centerX, spacing 10 ]
                [ Input.button
                    ( [ Border.solid
                      , Border.width 1
                      , Border.rounded 5
                      , Background.color white
                      , padding 20
                      , boxShadow
                      , centerX
                      ] ++ sceneComputationAttrs
                    )
                    { onPress = Just (ShowDetailedView id)
                    , label =
                        row [ spacing 20, centerX ]
                            [ el [] (text (notEditedValue name))
                            , Input.button []
                                { onPress = Just (AskDeleteScene id)
                                , label = el [ alignRight ] clearIcon
                                }
                            ]
                    }
                , arrowView id
                ]

        _ ->
            none



-- ** arrow view


arrowView : Uuid.Uuid -> Element Msg
arrowView id =
    let
        addSceneBtn =
            Input.button [ centerX ]
            { onPress = Just (ShowHttpRequestSelectionModal (Just id))
            , label = row [] [ addIcon
                             , text " add request"
                             ]
            }
    in
    el [ centerX
       , onRight addSceneBtn
       ] arrowDownwardIcon


-- ** detailed scene view


detailedSceneView : Model -> Scene -> RequestFileRecord -> Element Msg
detailedSceneView model scene requestFileRecord =
    let
        { scheme, method, headers, url, body } =
            buildRequestComputationInput model.keyValues requestFileRecord

        methodAndUrl =
            (methodToString method)
            ++ " "
            ++ (schemeToString scheme)
            ++ "://"
            ++ url

        inputSceneDetailView =
            [ column [ spacing 10 ]
                [ link []
                      { url = href <| ReqPage (Just scene.requestFileNodeId) (Just model.id)
                      , label = el [] <| iconWithTextAndColor "label" (editedOrNotEditedValue requestFileRecord.name)
                                secondaryColor
                      }
                , el [ centerX ] (text methodAndUrl)
                ]
            ]

        whichResponseButtonView : List (String, Bool, Msg) -> Element Msg
        whichResponseButtonView tabs =
            let
                buttonView (label, isActive, msg) =
                    Input.button [ centerX, centerY
                                 , height fill
                                 ]
                        { onPress = Just msg
                        , label =
                            el ( [ centerY, centerX
                                 , height fill
                                 ] ++ (selectiveButtonAttrs isActive)
                               ) <| el [ centerY ] (text label)
                        }
            in
            row [ width fill, height (px 50)
                , centerX, centerY
                , spacing 20
                , paddingXY 0 0
                ] <| List.map buttonView tabs

        outputSceneDetailView : SceneComputation -> List (Element Msg)
        outputSceneDetailView sceneComputation =
            case sceneComputation of
                SceneNotRun ->
                    [ none ]

                SceneRun (RequestComputationFailed httpException) ->
                    [ text <| "This request failed because of: " ++ httpExceptionToString httpException ]

                SceneRun (RequestComputationSucceeded requestComputationOutput) ->
                    [ statusResponseView requestComputationOutput
                    , whichResponseButtonView
                      [ ("Body", model.whichResponseView == BodyResponseView, ShowBodyResponseView)
                      , ("Headers", model.whichResponseView == HeaderResponseView, ShowHeaderResponseView)
                      ]
                    , case model.whichResponseView of
                          BodyResponseView ->
                              bodyResponseView requestComputationOutput (always DoNothing)

                          HeaderResponseView ->
                              headersResponseView requestComputationOutput (always DoNothing)
                    ]
    in
    column [ width fill
           , height fill
           , centerX
           , alignTop
           , spacing 40
           , padding 20
           , boxShadow
           , Background.color white
           ] <|
        case scene.computationOutput of
            Nothing ->
                inputSceneDetailView

            Just sceneComputation ->
                inputSceneDetailView
                ++ [ hr [] "response" ]
                ++ (outputSceneDetailView sceneComputation)


-- ** util


labelInputView : String -> Input.Label Msg
labelInputView labelText =
    let
        size =
            width
                (fill
                    |> maximum 100
                    |> minimum 100
                )
    in
    Input.labelAbove [ centerY, size ] <| text labelText


-- * modal


selectHttpRequestModal : Maybe Uuid -> RequestCollection -> Modal.Config Msg
selectHttpRequestModal sceneParentId requestCollection =
    let
        (RequestCollection _ requestNodes) =
            requestCollection

        treeView =
            column [ spacing 10 ] (nodeView requestNodes)

        nodeView : List RequestNode -> List (Element Msg)
        nodeView nodes =
            case nodes of
                [] ->
                    []

                node :: tail ->
                    case node of
                        RequestFolder { id, name, open, children } ->
                            let
                                folderChildrenView =
                                    nodeView children

                                tailView =
                                    nodeView tail

                                currentFolderView =
                                    folderView id name folderChildrenView open
                            in
                            currentFolderView :: tailView

                        RequestFile requestFileRecord ->
                            let
                                tailView =
                                    nodeView tail

                                currentFileView =
                                    Input.button []
                                        { onPress = Just (GenerateRandomUUIDForScene sceneParentId requestFileRecord.id)
                                        , label = el [] <| iconWithTextAndColor "label" (notEditedValue requestFileRecord.name) secondaryColor
                                        }
                            in
                            currentFileView :: tailView

        folderView : Uuid.Uuid -> Editable String -> List (Element Msg) -> Bool -> Element Msg
        folderView id name folderChildrenView open =
            let
                folderIcon =
                    case open of
                        False ->
                            "keyboard_arrow_right"

                        True ->
                            "keyboard_arrow_down"
            in
            column []
                [ iconWithText folderIcon (notEditedValue name)
                , case open of
                    True ->
                        column [ spacing 10, paddingXY 20 10 ] folderChildrenView

                    False ->
                        none
                ]
    in
    { closeMessage = CloseModal
    , header = text "Select an http request"
    , body = Just treeView
    , footer = Nothing
    }


confirmDeleteFolderModal : Modal.Config Msg
confirmDeleteFolderModal =
    { closeMessage = CloseModal
    , header = text "confirm delete folder"
    , body = Nothing
    , footer = Nothing
    }
