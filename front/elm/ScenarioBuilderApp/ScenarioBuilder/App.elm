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


-- * model


type alias Model =
    { notification : Maybe String
    , whichModal : Maybe Modal
    , id : Uuid.Uuid
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
  | GenerateRandomUUIDForScene RequestFileRecord
  | SelectRequestFile RequestFileRecord Uuid.Uuid
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

        GenerateRandomUUIDForScene requestFileRecord ->
            let
                newMsg = Random.generate (SelectRequestFile requestFileRecord) Uuid.uuidGenerator
            in
                (model, newMsg)

        SelectRequestFile requestFileRecord newSceneId ->
            let
                newScene =
                    mkDefaultScene newSceneId requestFileRecord

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


mkDefaultScene : Uuid.Uuid -> RequestFileRecord -> Scene
mkDefaultScene id requestFileRecord =
    { id = id
    , requestFileRecord = requestFileRecord
    }


-- * view


view : Model -> Element Msg
view model =
    case Debug.log "test2" model.scenes of
        [] ->
            addNewSceneView

        scenes ->
            column [] [ column [] (List.map sceneView scenes)
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


sceneView : Scene -> Element Msg
sceneView { requestFileRecord } =
    el [] (text (notEditedValue requestFileRecord.name))


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
                                { onPress = Just (GenerateRandomUUIDForScene requestFileRecord)
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
