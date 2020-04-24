module ScenarioBuilderApp.ScenarioBuilder.App exposing (..)

import Element exposing (..)
import Element.Background as Background
import Random
import List.Extra as List
import Element.Border as Border
import Element.Font as Font
import Element.Events as Events
import Element.Input as Input
import Application.Type exposing (..)
import Util exposing (..)
import Http
import Uuid exposing (Uuid)
import Modal exposing (Modal(..))
import Modal
import Api.Generated as Client
import Api.Converter as Client
import RequestBuilderApp.RequestTree.Util as RequestTree
import RequestComputation exposing(..)


-- * model


type alias Model =
    { session: Session
    , notification : Maybe String
    , whichModal : Maybe Modal
    , id : Uuid.Uuid
    , requestCollection : RequestCollection
    , scenarioCollectionId : Uuid
    , scenes : List Scene
    , keyValues : List (Storable NewKeyValue KeyValue)
    , name : Editable String
    , showDetailedSceneView : Maybe Uuid
    }


-- * message


type Msg
  -- create scene
  = ShowHttpRequestSelectionModal (Maybe Uuid)
  | GenerateRandomUUIDForScene (Maybe Uuid) Uuid.Uuid
  | SelectRequestFile (Maybe Uuid) Uuid.Uuid Uuid.Uuid
  | AskCreateScene (Maybe Uuid) Uuid Uuid
  | CloseModal
  -- delete scene
  | AskDeleteScene Uuid.Uuid
  | DeleteScene Uuid.Uuid
  | ServerError
  -- scenario
  | AskRunScenario
  | ScenarioProcessed ScenarioComputationOutput


-- * update


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of


-- ** create scene


        ShowHttpRequestSelectionModal sceneParentId ->
            let
                newModel =
                    { model | whichModal = Just (SelectHttpRequestModal sceneParentId) }
            in
                (newModel, Cmd.none)

        GenerateRandomUUIDForScene sceneParentId requestFileNodeId ->
            let
                newMsg =
                    Random.generate (AskCreateScene sceneParentId requestFileNodeId) Uuid.uuidGenerator
            in
                (model, newMsg)

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
                (model, newMsg)

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
                (newModel, Cmd.none)


-- ** delete scene


        AskDeleteScene sceneId ->
            let
                newMsg =
                    Client.deleteApiScenarioNodeByScenarioNodeIdSceneBySceneId "" (getCsrfToken model.session) model.id sceneId (deleteSceneResultToMsg sceneId)
            in
                (model, newMsg)

        DeleteScene id ->
            let
                newScenes =
                    List.filter (not << \scene -> scene.id == id) model.scenes

                newModel =
                    { model | scenes = newScenes }
            in
                (newModel, Cmd.none)


-- ** scenario


        AskRunScenario ->
            let
                findFileRecord : Uuid -> Maybe RequestFileRecord
                findFileRecord id =
                    let
                        (RequestCollection _ requestNodes) =
                            model.requestCollection
                    in
                        RequestTree.findFile requestNodes id

                sceneToInputScene : Scene -> Client.InputScene
                sceneToInputScene scene =
                    let
                        requestComputationInput = Maybe.map (buildRequestComputationInput model.keyValues) (findFileRecord scene.requestFileNodeId)
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
                (model, newMsg)

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
                (newModel, Cmd.none)


-- ** other


        CloseModal ->
            let
                newModel =
                    { model | whichModal = Nothing }
            in
                (newModel, Cmd.none)

        ServerError ->
            (model, Cmd.none)


-- * util


mkDefaultScene : Uuid.Uuid -> Uuid.Uuid -> Scene
mkDefaultScene  id requestFileNodeId =
    { id = id
    , requestFileNodeId = requestFileNodeId
    , computationOutput = Nothing
    }

runScenarioResultToMsg : Result Http.Error Client.ScenarioComputationOutput -> Msg
runScenarioResultToMsg result =
    case Debug.log "test" result of
        Ok scenarioComputationOutput ->
            ScenarioProcessed (Client.convertScenarioComputationOutputFromBackToFront scenarioComputationOutput)

        Err error ->
            Debug.todo "server error"  ServerError

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

    in
        wrappedRow [ width fill, centerX ]
            [ el [ width (fillPortion 8) ] scenesView
            , el [ width (fillPortion 2) ] scenarioSettingView
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
            })


-- ** scene view


sceneView : Model -> Scene -> Element Msg
sceneView model { id, requestFileNodeId, computationOutput } =
    let
        (RequestCollection _ requestNodes) =
            model.requestCollection

        mRequestFileRecord =
            RequestTree.findFile requestNodes requestFileNodeId

        sceneComputationColor =
            case computationOutput of
                Nothing -> Border.color white
                Just SceneNotRun -> Border.color primaryColor
                Just (SceneRun _) -> Border.color secondaryColor

    in
        case mRequestFileRecord of
            Just { name } ->
                column [ centerX, spacing 10 ]
                    [ Input.button
                          [ Border.solid
                          , Border.width 1
                          , Border.rounded 5
                          , sceneComputationColor
                          , Background.color white
                          , padding 20
                          , boxShadow
                          , centerX
                          ] { onPress = Just (AskDeleteScene id)
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

            _ -> none


-- ** arrow view


arrowView : Uuid.Uuid -> Element Msg
arrowView id =
    Input.button [ centerX ]
        { onPress = Just (ShowHttpRequestSelectionModal (Just id))
        , label = arrowDownwardIcon
        }



-- ** detailed scene view


detailedSceneView : Model -> Scene -> Element Msg
detailedSceneView model scene =
    none


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
                [] -> []
                node :: tail ->
                  case node of
                    (RequestFolder { id, name, open, children }) ->
                      let
                        folderChildrenView = nodeView children
                        tailView = nodeView tail
                        currentFolderView =
                            folderView id name folderChildrenView open
                      in
                        currentFolderView :: tailView

                    RequestFile requestFileRecord ->
                      let
                        tailView = nodeView tail
                        currentFileView =
                            Input.button []
                                { onPress = Just (GenerateRandomUUIDForScene sceneParentId requestFileRecord.id)
                                , label = el [] <| iconWithTextAndColor "label" (notEditedValue requestFileRecord.name) secondaryColor
                                }
                      in
                        currentFileView :: tailView

        folderView : Uuid.Uuid -> Editable String -> List(Element Msg) -> Bool -> Element Msg
        folderView id name folderChildrenView open =
            let
                folderIcon =
                    case open of
                        False -> "keyboard_arrow_right"
                        True -> "keyboard_arrow_down"
            in
                column [] [ iconWithText folderIcon (notEditedValue name)
                          , case open of
                                True -> column [ spacing 10, paddingXY 20 10 ] folderChildrenView
                                False -> none
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
