module ScenarioBuilderApp.ScenarioBuilder.App exposing (..)

import Element exposing (..)
import Element.Background as Background
import Random
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
import RequestBuilderApp.RequestTree.Util as RequestTree


-- * model


type alias Model =
    { session: Session
    , notification : Maybe String
    , whichModal : Maybe Modal
    , id : Uuid.Uuid
    , requestCollection : RequestCollection
    , scenarioCollectionId : Uuid.Uuid
    , scenes : List Scene
    , keyValues : List (Storable NewKeyValue KeyValue)
    , name : Editable String
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
    }

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
    case model.scenes of
        [] ->
            addNewSceneView

        scenes ->
            column [ centerX, spacing 10 ] (List.map (sceneView model) scenes)


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
sceneView model { id, requestFileNodeId } =
    let
        (RequestCollection _ requestNodes) =
            model.requestCollection

        mRequestFileRecord =
            RequestTree.findFile requestNodes requestFileNodeId
    in
        case mRequestFileRecord of
            Just { name } ->
                column [ centerX, spacing 10 ]
                    [ el [ Border.solid
                    , Border.width 1
                    , Border.rounded 5
                    , Border.color white
                    , Background.color white
                    , padding 20
                    , boxShadow
                    , centerX
                    ] <| row [ spacing 20, centerX ]
                            [ el [] (text (notEditedValue name))
                            , Input.button []
                                { onPress = Just (AskDeleteScene id)
                                , label = el [ alignRight ] clearIcon
                                }
                            ]
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
