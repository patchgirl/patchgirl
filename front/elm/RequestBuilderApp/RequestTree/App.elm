module RequestBuilderApp.RequestTree.App exposing (..)

--import RequestBuilderApp.RequestBuilder.App as RequestBuilder
import Application.Type exposing (..)
import Random
import Uuid
import Http
import Api.Generated as Client

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Util exposing (..)
import Page exposing(..)
import Animation


-- * model


type alias Model a =
    { a
        | requestCollection : RequestCollection
        , displayedRequestNodeMenuId : Maybe Uuid.Uuid
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
    }


-- * message


type Msg
  = ToggleFolder Uuid.Uuid
  | ToggleMenu Uuid.Uuid
  -- mkdir
  | GenerateRandomUUIDForFolder Uuid.Uuid
  | AskMkdir Uuid.Uuid Uuid.Uuid
  | Mkdir Uuid.Uuid Uuid.Uuid
  -- create file
  | GenerateRandomUUIDForFile Uuid.Uuid
  | AskTouch Uuid.Uuid Uuid.Uuid
  | Touch Uuid.Uuid Uuid.Uuid
  -- create root file
  | GenerateRandomUUIDForRootFile
  | AskTouchRoot Uuid.Uuid
  | TouchRoot Uuid.Uuid
  -- create root folder
  | GenerateRandomUUIDForRootFolder
  | AskMkdirRoot Uuid.Uuid
  | MkdirRoot Uuid.Uuid
  -- rename
  | ShowRenameInput Uuid.Uuid
  | ChangeName Uuid.Uuid String -- while focus is on the input
  | AskRename Uuid.Uuid String  -- validate input
  | Rename Uuid.Uuid String     -- refresh input
  -- delete
  | AskDelete Uuid.Uuid
  | Delete Uuid.Uuid
  | DoNothing
  | BuilderTreeServerError


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
  case msg of


-- ** toggle builder/menu/folder


    ToggleMenu id ->
        let
            newDisplayedRequestNodeMenuIndex =
                case maybeExists model.displayedRequestNodeMenuId ((==) id) of
                    True -> Nothing -- menu already displayed
                    False -> Just id

            newModel =
                { model |
                      displayedRequestNodeMenuId = newDisplayedRequestNodeMenuIndex
                }
        in
            (newModel, Cmd.none)

    ToggleFolder id ->
        let
            (RequestCollection requestCollectionId requestNodes) =
                model.requestCollection

            newRequestNodes =
                List.map (modifyRequestNode id toggleFolder) requestNodes

            newModel =
                { model
                    | requestCollection =
                      RequestCollection requestCollectionId newRequestNodes
                }
        in
            (newModel, Cmd.none)


-- ** mkdir


    GenerateRandomUUIDForFolder parentNodeId ->
        let
            newMsg = Random.generate (AskMkdir parentNodeId) Uuid.uuidGenerator
        in
            (model, newMsg)

    AskMkdir parentNodeId newId ->
        let
            (RequestCollection requestCollectionId requestNodes) =
                model.requestCollection

            newRequestFolder =
                { newRequestFolderId = newId
                , newRequestFolderParentNodeId = parentNodeId
                , newRequestFolderName = "new folder"
                }

            newMsg =
                Client.postApiRequestCollectionByRequestCollectionIdRequestFolder "" "" requestCollectionId newRequestFolder (createRequestFolderResultToMsg parentNodeId newId)
        in
            (model, newMsg)

    Mkdir parentNodeId newId ->
        let
            (RequestCollection id requestNodes) =
                model.requestCollection

            newRequestNodes =
                List.map (modifyRequestNode parentNodeId (mkdir newId)) requestNodes

            newModel =
                { model
                    | requestCollection =
                      RequestCollection id newRequestNodes
                }
        in
            (newModel, Cmd.none)


-- ** touch


    GenerateRandomUUIDForFile parentNodeId ->
        let
            newMsg = Random.generate (AskTouch parentNodeId) Uuid.uuidGenerator
        in
            (model, newMsg)

    AskTouch parentNodeId newId ->
        let
            (RequestCollection requestCollectionId requestNodes) =
                model.requestCollection

            newRequestFile =
                   { newRequestFileId = newId
                   , newRequestFileParentNodeId = parentNodeId
                   }

            newMsg =
                Client.postApiRequestCollectionByRequestCollectionIdRequestFile "" "" requestCollectionId newRequestFile (newRequestFileResultToMsg parentNodeId newId)
        in
            (model, newMsg)

    Touch parentNodeId newId ->
        let
            (RequestCollection id requestNodes) = model.requestCollection

            newRequestNodes =
                List.map (modifyRequestNode parentNodeId (touch newId)) requestNodes

            newModel =
                { model
                    | requestCollection =
                      RequestCollection id newRequestNodes
                }
        in
            (newModel, Cmd.none)


-- ** rename


    ShowRenameInput id ->
        let
            (RequestCollection requestCollectionId requestNodes) =
                model.requestCollection

            newRequestNodes =
                List.map (modifyRequestNode id displayRenameInput) requestNodes

            newModel =
                { model
                    | requestCollection =
                      RequestCollection requestCollectionId newRequestNodes
                }
        in
            (newModel, Cmd.none)

    ChangeName id newName ->
        let
            (RequestCollection requestCollectionId requestNodes) = model.requestCollection

            newRequestNodes =
                List.map (modifyRequestNode id (tempRename newName)) requestNodes

            newModel =
                { model
                    | requestCollection =
                      RequestCollection requestCollectionId newRequestNodes
                }
        in
            (newModel, Cmd.none)

    AskRename id newName ->
        let
            (RequestCollection requestCollectionId requestNodes) =
                model.requestCollection

            payload =
                Client.UpdateRequestNode { updateRequestNodeName = newName }

            newMsg =
                Client.putApiRequestCollectionByRequestCollectionIdRequestNodeByRequestNodeId "" "" requestCollectionId id payload (renameNodeResultToMsg id newName)
        in
            (model, newMsg)

    Rename id newName ->
        let
            (RequestCollection requestCollectionId requestNodes) =
                model.requestCollection

            newRequestNodes =
                List.map (modifyRequestNode id (rename newName)) requestNodes

            newModel =
                { model
                    | requestCollection =
                      RequestCollection requestCollectionId newRequestNodes
                }
        in
            (newModel, Cmd.none)


-- ** delete


    AskDelete id ->
        let
            (RequestCollection requestCollectionId _) = model.requestCollection

            newMsg =
                Client.deleteApiRequestCollectionByRequestCollectionIdRequestNodeByRequestNodeId "" "" requestCollectionId id (deleteRequestNodeResultToMsg id)
        in
            (model, newMsg)

    Delete id ->
        let
            (RequestCollection requestCollectionId requestNodes) = model.requestCollection

            newRequestNodes =
                List.concatMap (deleteRequestNode id) requestNodes

            newModel =
                { model
                    | requestCollection =
                        RequestCollection requestCollectionId newRequestNodes
                }
        in
            (newModel, Cmd.none)

-- ** root file


    GenerateRandomUUIDForRootFile ->
        let
            newMsg = Random.generate AskTouchRoot Uuid.uuidGenerator
        in
            (model, newMsg)

    AskTouchRoot newId ->
        let
            (RequestCollection requestCollectionId _) =
                model.requestCollection

            newRootRequestFile =
                Client.NewRootRequestFile { newRootRequestFileId = newId }

            newMsg =
                Client.postApiRequestCollectionByRequestCollectionIdRootRequestFile "" "" requestCollectionId newRootRequestFile (createRootRequestFileResultToMsg newId)
        in
            (model, newMsg)

    TouchRoot newId ->
        let
            (RequestCollection requestCollectionId requestNodes) =
                model.requestCollection

            newRequestNodes =
                requestNodes ++ [ mkDefaultFile newId ]

            newModel =
                { model
                    | requestCollection =
                        RequestCollection requestCollectionId newRequestNodes
                }
        in
            (newModel, Cmd.none)


-- ** root folder


    GenerateRandomUUIDForRootFolder ->
        let
            newMsg = Random.generate AskMkdirRoot Uuid.uuidGenerator
        in
            (model, newMsg)

    AskMkdirRoot newId ->
        let
            (RequestCollection requestCollectionId _) =
                model.requestCollection

            newRootRequestFolder =
                Client.NewRootRequestFolder { newRootRequestFolderId = newId }

            newMsg =
                Client.postApiRequestCollectionByRequestCollectionIdRootRequestFolder "" "" requestCollectionId newRootRequestFolder (createRootRequestFolderResultToMsg newId)
        in
            (model, newMsg)

    MkdirRoot newId ->
        let
            (RequestCollection requestCollectionId requestNodes) =
                model.requestCollection

            newRequestNodes =
                requestNodes ++ [ mkDefaultFolder newId ]

            newModel =
                { model
                    | requestCollection =
                        RequestCollection requestCollectionId newRequestNodes
                }
        in
            (newModel, Cmd.none)


-- ** other


    BuilderTreeServerError ->
        Debug.todo "error with builder tree"


    DoNothing ->
        (model, Cmd.none)


-- * util


-- ** msg handler


renameNodeResultToMsg : Uuid.Uuid -> String -> Result Http.Error () -> Msg
renameNodeResultToMsg id newName result =
    case result of
        Ok _ ->
            Rename id newName

        Err error ->
            BuilderTreeServerError

createRequestFolderResultToMsg : Uuid.Uuid -> Uuid.Uuid -> Result Http.Error () -> Msg
createRequestFolderResultToMsg parentNodeId id result =
    case result of
        Ok _ ->
            Mkdir parentNodeId id

        Err error ->
            BuilderTreeServerError

deleteRequestNodeResultToMsg : Uuid.Uuid -> Result Http.Error () -> Msg
deleteRequestNodeResultToMsg id result =
    case result of
        Ok _ ->
            Delete id

        Err error ->
            BuilderTreeServerError

newRequestFileResultToMsg : Uuid.Uuid -> Uuid.Uuid -> Result Http.Error () -> Msg
newRequestFileResultToMsg parentNodeId id result =
    case result of
        Ok _ ->
            Touch parentNodeId id

        Err error ->
            BuilderTreeServerError

createRootRequestFileResultToMsg : Uuid.Uuid -> Result Http.Error () -> Msg
createRootRequestFileResultToMsg id result =
    case result of
        Ok _ ->
            TouchRoot id

        Err error ->
            BuilderTreeServerError

createRootRequestFolderResultToMsg : Uuid.Uuid -> Result Http.Error () -> Msg
createRootRequestFolderResultToMsg id result =
    case result of
        Ok _ ->
            MkdirRoot id

        Err error ->
            BuilderTreeServerError


-- ** tree manipulation

getRequestNodeId : RequestNode -> Uuid.Uuid
getRequestNodeId requestNode =
    case requestNode of
        RequestFolder { id } -> id
        RequestFile { id } -> id

findFile : List RequestNode -> Uuid.Uuid -> Maybe RequestFileRecord
findFile requestNodes id =
    case findNode requestNodes id of
        Just (RequestFile file) ->
            Just file

        _ ->
            Nothing

findNode : List RequestNode -> Uuid.Uuid -> Maybe RequestNode
findNode requestNodes id =
    let
        find : RequestNode -> Maybe RequestNode
        find requestNode =
            case requestNode of
                (RequestFile file) as node ->
                    case file.id == id of
                        True ->
                            Just node

                        False ->
                            Nothing

                (RequestFolder folder) as node ->
                    case folder.id == id of
                        True ->
                            Just node

                        False ->
                            findNode folder.children id
    in
        List.head <| catMaybes (List.map find requestNodes)

modifyRequestNode : Uuid.Uuid -> (RequestNode -> RequestNode) -> RequestNode -> RequestNode
modifyRequestNode id f requestNode =
    case getRequestNodeId requestNode == id of
        True -> f requestNode
        False ->
            case requestNode of
                RequestFile requestFile ->
                    RequestFile requestFile

                RequestFolder requestFolder ->
                    RequestFolder { requestFolder
                                      | children =
                                        List.map (modifyRequestNode id f) requestFolder.children
                                  }

deleteRequestNode : Uuid.Uuid -> RequestNode -> List RequestNode
deleteRequestNode idToDelete requestNode =
    case getRequestNodeId requestNode == idToDelete of
        True -> []
        False ->
            case requestNode of
                RequestFile requestFile ->
                    [RequestFile requestFile]

                RequestFolder requestFolder ->
                    [ RequestFolder { requestFolder
                                      | children =
                                        List.concatMap (deleteRequestNode idToDelete) requestFolder.children
                                  }
                    ]

toggleFolder : RequestNode -> RequestNode
toggleFolder node =
  case node of
    RequestFile _ as file -> file
    RequestFolder folder ->
        RequestFolder { folder
                          | open = (not folder.open)
                      }

mkdir : Uuid.Uuid -> RequestNode -> RequestNode
mkdir id node =
  case node of
    RequestFile _ as file -> file
    RequestFolder folder ->
        RequestFolder { folder
                          | children = mkDefaultFolder id :: folder.children
                          , open = True
                      }

touch : Uuid.Uuid -> RequestNode -> RequestNode
touch id parentNode =
  case parentNode of
    RequestFile _ as file -> file
    RequestFolder folder ->
      RequestFolder { folder
                        | children = mkDefaultFile id  :: folder.children
                        , open = True
                    }

displayRenameInput : RequestNode -> RequestNode
displayRenameInput node =
  case node of
    RequestFolder folder ->
        let
            oldValue = notEditedValue folder.name
        in
            RequestFolder { folder | name = Edited oldValue oldValue }

    RequestFile file ->
        let
            oldValue = notEditedValue file.name
        in
            RequestFile { file | name = Edited oldValue oldValue }

rename : String -> RequestNode -> RequestNode
rename newName node =
  case node of
    RequestFolder folder ->
        RequestFolder { folder | name = NotEdited newName }
    RequestFile file ->
        RequestFile { file | name = NotEdited newName }

tempRename : String -> RequestNode -> RequestNode
tempRename newName node =
    case node of
        RequestFolder folder ->
            RequestFolder { folder | name = changeEditedValue newName folder.name }

        RequestFile file ->
            RequestFile { file | name = changeEditedValue newName file.name }

mkDefaultFolder : Uuid.Uuid -> RequestNode
mkDefaultFolder id =
    RequestFolder { id = id
                  , name = NotEdited "new folder"
                  , open = False
                  , children = []
                  }

mkDefaultFile : Uuid.Uuid -> RequestNode
mkDefaultFile id =
    RequestFile { id = id
                , name = NotEdited "new request"
                , httpUrl = NotEdited ""
                , httpMethod = NotEdited HttpGet
                , httpHeaders = NotEdited []
                , httpBody = NotEdited ""
                , showResponseView = False
                , requestComputationResult = Nothing
                , runRequestIconAnimation = Animation.style []
                }


-- * view


view : Model a -> Element Msg
view model =
    let
        (RequestCollection _ requestNodes) = model.requestCollection

        mainMenuView =
            row [ spacing 10 ]
                [ Input.button []
                      { onPress = Just <| GenerateRandomUUIDForRootFolder
                      , label = iconWithText "create_new_folder" "new folder"
                      }
                , Input.button []
                      { onPress = Just <| GenerateRandomUUIDForRootFile
                      , label = iconWithText "note_add" "new request"
                      }
                ]

        treeView =
            column [ spacing 10 ] (nodeView model.displayedRequestNodeMenuId requestNodes)

    in
        column [ alignTop, spacing 20, centerX ]
            [ mainMenuView
            , treeView
            ]



nodeView : Maybe Uuid.Uuid -> List RequestNode -> List (Element Msg)
nodeView mDisplayedRequestNodeMenuIndex requestCollection =
    case requestCollection of
      [] -> []
      node :: tail ->
        case node of
          (RequestFolder { id, name, open, children }) ->
            let
              folderChildrenView = nodeView mDisplayedRequestNodeMenuIndex children
              tailView = nodeView mDisplayedRequestNodeMenuIndex tail
              currentFolderView =
                  folderView id mDisplayedRequestNodeMenuIndex name folderChildrenView open
            in
              currentFolderView :: tailView

          (RequestFile { id, name }) ->
            let
              tailView = nodeView mDisplayedRequestNodeMenuIndex tail
              currentFileView = fileView id mDisplayedRequestNodeMenuIndex name
            in
              currentFileView :: tailView

-- ** file view


fileReadView : String -> Uuid.Uuid -> Element Msg
fileReadView name id =
    link []
            { url = href (ReqPage (Just id))
            , label = el [] <| iconWithTextAndColor "label" name secondaryColor
            }

fileEditView : String -> Uuid.Uuid -> Element Msg
fileEditView name id =
  Input.text
      [ Util.onEnterWithInput (AskRename id)
      ]
      { onChange = ChangeName id
      , text = name
      , placeholder = Nothing
      , label = Input.labelHidden "rename file"
      }

fileView : Uuid.Uuid -> Maybe Uuid.Uuid -> Editable String -> Element Msg
fileView id mDisplayedRequestNodeMenuIndex name =
    let
        modeView =
            case name of
                NotEdited value -> fileReadView value id
                Edited oldValue newValue -> fileEditView newValue id

        showMenu =
            mDisplayedRequestNodeMenuIndex == Just id

        menuView =
            case not showMenu of
                True -> none
                False ->
                    row []
                        [ Input.button []
                              { onPress = Just <| ShowRenameInput id
                              , label = editIcon
                              }
                        , Input.button []
                            { onPress = Just <| AskDelete id
                            , label = deleteIcon
                            }
                        ]
  in
      row []
          [ modeView
          , Input.button []
              { onPress = Just <| ToggleMenu id
              , label =
                  icon <|
                      case showMenu of
                          True -> "more_horiz"
                          False -> "more_vert"
              }
          , menuView
          ]


-- ** folder view


folderWithIconView : String -> Bool -> Element Msg
folderWithIconView name isOpen =
    let
        folderIconText =
            case isOpen of
                False -> "keyboard_arrow_right"
                True -> "keyboard_arrow_down"
    in
        iconWithText folderIconText name

folderMenuView : Uuid.Uuid -> Bool -> Element Msg
folderMenuView id isOpen =
    let
        iconClass =
            case isOpen of
                True -> "more_horiz"
                False -> "more_vert"
        menuIcon = icon iconClass
        menuView =
            row [ spacing 5 ]
                [ Input.button []
                      { onPress = Just <| ShowRenameInput id
                      , label = editIcon
                      }
                , Input.button []
                    { onPress = Just <| GenerateRandomUUIDForFolder id
                    , label = createFolderIcon
                    }
                , Input.button []
                    { onPress = Just <| GenerateRandomUUIDForFile id
                    , label = createFileIcon
                    }
                , Input.button []
                    { onPress = Just <| AskDelete id
                    , label = deleteIcon
                    }
                ]
    in
        case isOpen of
            True -> row [] [ menuIcon, menuView ]
            False -> row [] [ menuIcon ]


folderReadView : Uuid.Uuid -> String -> Bool -> Element Msg
folderReadView id name isOpen =
    Input.button []
        { onPress = Just <| ToggleFolder id
        , label = folderWithIconView name isOpen
        }

folderEditView : Uuid.Uuid -> String -> Element Msg
folderEditView id name =
  Input.text
      [ Util.onEnterWithInput (AskRename id)
      ]
      { onChange = ChangeName id
      , text = name
      , placeholder = Nothing
      , label = Input.labelHidden "rename folder"
      }

folderView : Uuid.Uuid -> Maybe Uuid.Uuid -> Editable String -> List(Element Msg) -> Bool -> Element Msg
folderView id mDisplayedRequestNodeMenuIndex name folderChildrenView open =
    let
        modeView =
            case name of
                NotEdited value ->
                    folderReadView id value open
                Edited oldValue newValue ->
                    folderEditView id newValue

        showMenu =
            Just id == mDisplayedRequestNodeMenuIndex
    in
        column [ width (fill |> maximum 300) ]
            [ row []
                  [ modeView
                  , Input.button []
                      { onPress = Just <| ToggleMenu id
                      , label = folderMenuView id showMenu
                      }
                  ]
            , case open of
                  True -> column [ spacing 10, paddingXY 20 10 ] folderChildrenView
                  False -> none
            ]
