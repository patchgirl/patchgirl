module ScenarioBuilderApp.ScenarioBuilder.App exposing (..)

import Element exposing (..)
import Element.Background as Background
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
    , keyValues : List (Storable NewKeyValue KeyValue)
    , name : Editable String
    }


-- * message


type Msg
  = DoNothing
  -- modal: select http request
  | ShowHttpRequestSelectionModal
  | SelectRequestFile Uuid.Uuid
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

        CloseModal ->
            let
                newModel =
                    { model | whichModal = Nothing }
            in
                (newModel, Cmd.none)

        _ ->
            (model, Cmd.none)


-- * view


view : Model -> Element Msg
view model =
    el [ width fill, centerX ]
        (Input.button [ centerX ]
            { onPress = Just ShowHttpRequestSelectionModal
            , label = el [ centerX, centerY ] (iconWithTextAndColorAndAttr "send" "Select http request" primaryColor [])
            })


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

                    (RequestFile { id, name }) ->
                      let
                        tailView = nodeView tail
                        currentFileView =
                            Input.button []
                                { onPress = Just (SelectRequestFile id)
                                , label = el [] <| iconWithTextAndColor "label" (notEditedValue name) secondaryColor
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
        , body = Just  treeView
        , footer = Just (text "Confirm    Cancel")
        }

confirmDeleteFolderModal : Modal.Config Msg
confirmDeleteFolderModal =
    { closeMessage = CloseModal
    , header = text "confirm delete folder"
    , body = Nothing
    , footer = Nothing
    }
