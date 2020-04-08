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
import Uuid
import Modal exposing (Modal(..))
import Modal
import RequestBuilderApp.RequestTree.Util as RequestTree


-- * model


type alias Model =
    { notification : Maybe String
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
  = DoNothing
  -- modal: select http request
  | ShowHttpRequestSelectionModal
  | GenerateRandomUUIDForScene Uuid.Uuid
  | SelectRequestFile Uuid.Uuid Uuid.Uuid
  | CloseModal


-- * update


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ShowHttpRequestSelectionModal ->
            let
                newModel =
                    { model | whichModal = Just SelectHttpRequestModal }
            in
                (newModel, Cmd.none)

        GenerateRandomUUIDForScene requestFileNodeId ->
            let
                newMsg = Random.generate (SelectRequestFile requestFileNodeId) Uuid.uuidGenerator
            in
                (model, newMsg)

        SelectRequestFile requestFileNodeId newSceneId ->
            let
                newScene =
                    mkDefaultScene newSceneId requestFileNodeId

                newModel =
                    { model
                        | whichModal = Nothing
                        , scenes = model.scenes ++ [ newScene ]
                    }
            in
                (Debug.log "test" newModel, Cmd.none)

        CloseModal ->
            let
                newModel =
                    { model | whichModal = Nothing }
            in
                (newModel, Cmd.none)

        _ ->
            (model, Cmd.none)


-- * util


mkDefaultScene : Uuid.Uuid -> Uuid.Uuid -> Scene
mkDefaultScene id requestFileNodeId =
    { id = id
    , requestFileNodeId = requestFileNodeId
    }


-- * view


view : Model -> Element Msg
view model =
    case Debug.log "test2" model.scenes of
        [] ->
            addNewSceneView

        scenes ->
            column [] [ column [] (List.map (sceneView model) scenes)
                      , addNewSceneView
                      ]


-- ** add new scene view


addNewSceneView : Element Msg
addNewSceneView =
    el [ width fill, centerX ]
        (Input.button [ centerX ]
            { onPress = Just ShowHttpRequestSelectionModal
            , label = el [ centerX, centerY ] (iconWithTextAndColorAndAttr "send" "Select http request" primaryColor [])
            })


-- ** scene view


sceneView : Model -> Scene -> Element Msg
sceneView model { requestFileNodeId } =
    let
        (RequestCollection _ requestNodes) =
            model.requestCollection

        mRequestFileRecord =
            RequestTree.findFile requestNodes requestFileNodeId
    in
        case mRequestFileRecord of
            Just { name } ->
                el [] (text (notEditedValue name))

            _ -> none



-- * modal


selectHttpRequestModal : RequestCollection -> Modal.Config Msg
selectHttpRequestModal requestCollection =
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
                                { onPress = Just (GenerateRandomUUIDForScene requestFileRecord.id)
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
        , header = text "select http request"
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
