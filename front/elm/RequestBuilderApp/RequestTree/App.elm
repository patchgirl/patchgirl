module RequestBuilderApp.RequestTree.App exposing (..)

--import RequestBuilderApp.RequestBuilder.App as RequestBuilder

import Animation
import Api.WebGeneratedClient as Client
import Application.Type exposing (..)
import Element exposing (..)
import Element.Input as Input
import Element.Font as Font
import Http
import Page exposing (..)
import Random
import Util exposing (..)
import Uuid exposing (Uuid)
import Browser.Navigation as Navigation
import HttpError exposing(..)


-- * model


type alias Model a =
    { a
        | requestCollection : RequestCollection
        , notification : Maybe Notification
        , displayedRequestNodeMenuId : Maybe Uuid
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , displayedRequestId : Maybe Uuid
        , navigationKey : Navigation.Key
    }


-- * message


type Msg
    = ToggleFolder Uuid
    | ToggleMenu Uuid
      -- mkdir
    | GenerateRandomUUIDForFolder Uuid
    | AskMkdir Uuid Uuid
    | Mkdir Uuid Uuid
      -- create file
    | GenerateRandomUUIDForFile Uuid
    | AskTouch Uuid Uuid
    | Touch Uuid Uuid
      -- create root file
    | GenerateRandomUUIDForRootFile
    | AskTouchRoot Uuid
    | TouchRoot Uuid
      -- create root folder
    | GenerateRandomUUIDForRootFolder
    | AskMkdirRoot Uuid
    | MkdirRoot Uuid
      -- rename
    | ShowRenameInput Uuid
    | ChangeName Uuid String -- while focus is on the input
    | AskRename Uuid String -- validate input
    | Rename Uuid String -- refresh input
      -- delete
    | AskDelete Uuid
    | Delete Uuid
    | PrintNotification Notification



-- * update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        ToggleMenu id ->
            let
                newDisplayedRequestNodeMenuIndex =
                    case maybeExists model.displayedRequestNodeMenuId ((==) id) of
                        True ->
                            Nothing

                        -- menu already displayed
                        False ->
                            Just id

                newModel =
                    { model
                        | displayedRequestNodeMenuId = newDisplayedRequestNodeMenuIndex
                    }
            in
            ( newModel, Cmd.none )

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
            ( newModel, Cmd.none )

        GenerateRandomUUIDForFolder parentNodeId ->
            let
                newMsg =
                    Random.generate (AskMkdir parentNodeId) Uuid.uuidGenerator
            in
            ( model, newMsg )

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
            ( model, newMsg )

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
            ( newModel, Cmd.none )

        GenerateRandomUUIDForFile parentNodeId ->
            let
                newMsg =
                    Random.generate (AskTouch parentNodeId) Uuid.uuidGenerator
            in
            ( model, newMsg )

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
            ( model, newMsg )

        Touch parentNodeId newId ->
            let
                (RequestCollection id requestNodes) =
                    model.requestCollection

                newRequestNodes =
                    List.map (modifyRequestNode parentNodeId (touch newId)) requestNodes

                newModel =
                    { model
                        | requestCollection =
                            RequestCollection id newRequestNodes
                    }
            in
            ( newModel, Cmd.none )

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
            ( newModel, Cmd.none )

        ChangeName id newName ->
            let
                (RequestCollection requestCollectionId requestNodes) =
                    model.requestCollection

                newRequestNodes =
                    List.map (modifyRequestNode id (tempRename newName)) requestNodes

                newModel =
                    { model
                        | requestCollection =
                            RequestCollection requestCollectionId newRequestNodes
                    }
            in
            ( newModel, Cmd.none )

        AskRename id newName ->
            let
                (RequestCollection requestCollectionId requestNodes) =
                    model.requestCollection

                payload =
                    Client.UpdateRequestNode { updateRequestNodeName = newName }

                newMsg =
                    Client.putApiRequestCollectionByRequestCollectionIdRequestNodeByRequestNodeId "" "" requestCollectionId id payload (renameNodeResultToMsg id newName)
            in
            ( model, newMsg )

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
            ( newModel, Cmd.none )

        AskDelete id ->
            let
                (RequestCollection requestCollectionId _) =
                    model.requestCollection

                newMsg =
                    Client.deleteApiRequestCollectionByRequestCollectionIdRequestNodeByRequestNodeId "" "" requestCollectionId id (deleteRequestNodeResultToMsg id)
            in
            ( model, newMsg )

        Delete id ->
            let
                (RequestCollection requestCollectionId requestNodes) =
                    model.requestCollection

                newRequestNodes =
                    List.concatMap (deleteRequestNode id) requestNodes

                newModel =
                    { model
                        | requestCollection =
                            RequestCollection requestCollectionId newRequestNodes
                    }

                newMsg =
                    case model.displayedRequestId == Just id of
                        True ->
                            Navigation.pushUrl model.navigationKey (href (ReqPage Nothing Nothing))

                        False ->
                            Cmd.none

            in
            ( newModel, newMsg )

        GenerateRandomUUIDForRootFile ->
            let
                newMsg =
                    Random.generate AskTouchRoot Uuid.uuidGenerator
            in
            ( model, newMsg )

        AskTouchRoot newId ->
            let
                (RequestCollection requestCollectionId _) =
                    model.requestCollection

                newRootRequestFile =
                    Client.NewRootRequestFile { newRootRequestFileId = newId }

                newMsg =
                    Client.postApiRequestCollectionByRequestCollectionIdRootRequestFile "" "" requestCollectionId newRootRequestFile (createRootRequestFileResultToMsg newId)
            in
            ( model, newMsg )

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
            ( newModel, Cmd.none )

        GenerateRandomUUIDForRootFolder ->
            let
                newMsg =
                    Random.generate AskMkdirRoot Uuid.uuidGenerator
            in
            ( model, newMsg )

        AskMkdirRoot newId ->
            let
                (RequestCollection requestCollectionId _) =
                    model.requestCollection

                newRootRequestFolder =
                    Client.NewRootRequestFolder { newRootRequestFolderId = newId }

                newMsg =
                    Client.postApiRequestCollectionByRequestCollectionIdRootRequestFolder "" "" requestCollectionId newRootRequestFolder (createRootRequestFolderResultToMsg newId)
            in
            ( model, newMsg )

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
            ( newModel, Cmd.none )

        PrintNotification notification ->
            ( { model | notification = Just notification }, Cmd.none )


-- * util


-- ** msg handler


renameNodeResultToMsg : Uuid -> String -> Result Http.Error () -> Msg
renameNodeResultToMsg id newName result =
    case result of
        Ok _ ->
            Rename id newName

        Err error ->
            PrintNotification <| AlertNotification "Could not rename, try reloading the page!" (httpErrorToString error)


createRequestFolderResultToMsg : Uuid -> Uuid -> Result Http.Error () -> Msg
createRequestFolderResultToMsg parentNodeId id result =
    case result of
        Ok _ ->
            Mkdir parentNodeId id

        Err error ->
            PrintNotification <| AlertNotification "Could not create folder, try reloading the page!" (httpErrorToString error)


deleteRequestNodeResultToMsg : Uuid -> Result Http.Error () -> Msg
deleteRequestNodeResultToMsg id result =
    case result of
        Ok _ ->
            Delete id

        Err error ->
            PrintNotification <| AlertNotification "Could not delete, maybe this HTTP request is used in a scenario? Check the scenario and try reloading the page!" (httpErrorToString error)


newRequestFileResultToMsg : Uuid -> Uuid -> Result Http.Error () -> Msg
newRequestFileResultToMsg parentNodeId id result =
    case result of
        Ok _ ->
            Touch parentNodeId id

        Err error ->
            PrintNotification <| AlertNotification "Could create a new file, try reloading the page" (httpErrorToString error)


createRootRequestFileResultToMsg : Uuid -> Result Http.Error () -> Msg
createRootRequestFileResultToMsg id result =
    case result of
        Ok _ ->
            TouchRoot id

        Err error ->
            PrintNotification <| AlertNotification "Could not create a new file, try reloading the page" (httpErrorToString error)


createRootRequestFolderResultToMsg : Uuid -> Result Http.Error () -> Msg
createRootRequestFolderResultToMsg id result =
    case result of
        Ok _ ->
            MkdirRoot id

        Err error ->
            PrintNotification <| AlertNotification "Could not create folder, try reloading the page" (httpErrorToString error)



-- ** tree manipulation


getRequestNodeId : RequestNode -> Uuid
getRequestNodeId requestNode =
    case requestNode of
        RequestFolder { id } ->
            id

        RequestFile { id } ->
            id


modifyRequestNode : Uuid -> (RequestNode -> RequestNode) -> RequestNode -> RequestNode
modifyRequestNode id f requestNode =
    case getRequestNodeId requestNode == id of
        True ->
            f requestNode

        False ->
            case requestNode of
                RequestFile requestFile ->
                    RequestFile requestFile

                RequestFolder requestFolder ->
                    RequestFolder
                        { requestFolder
                            | children =
                                List.map (modifyRequestNode id f) requestFolder.children
                        }


deleteRequestNode : Uuid -> RequestNode -> List RequestNode
deleteRequestNode idToDelete requestNode =
    case getRequestNodeId requestNode == idToDelete of
        True ->
            []

        False ->
            case requestNode of
                RequestFile requestFile ->
                    [ RequestFile requestFile ]

                RequestFolder requestFolder ->
                    [ RequestFolder
                        { requestFolder
                            | children =
                                List.concatMap (deleteRequestNode idToDelete) requestFolder.children
                        }
                    ]


toggleFolder : RequestNode -> RequestNode
toggleFolder node =
    case node of
        (RequestFile _) as file ->
            file

        RequestFolder folder ->
            RequestFolder
                { folder
                    | open = not folder.open
                }


mkdir : Uuid -> RequestNode -> RequestNode
mkdir id node =
    case node of
        (RequestFile _) as file ->
            file

        RequestFolder folder ->
            RequestFolder
                { folder
                    | children = mkDefaultFolder id :: folder.children
                    , open = True
                }


touch : Uuid -> RequestNode -> RequestNode
touch id parentNode =
    case parentNode of
        (RequestFile _) as file ->
            file

        RequestFolder folder ->
            RequestFolder
                { folder
                    | children = mkDefaultFile id :: folder.children
                    , open = True
                }


displayRenameInput : RequestNode -> RequestNode
displayRenameInput node =
    case node of
        RequestFolder folder ->
            let
                oldValue =
                    notEditedValue folder.name
            in
            RequestFolder { folder | name = Edited oldValue oldValue }

        RequestFile file ->
            let
                oldValue =
                    notEditedValue file.name
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


mkDefaultFolder : Uuid -> RequestNode
mkDefaultFolder id =
    RequestFolder
        { id = id
        , name = NotEdited "new folder"
        , open = False
        , children = []
        }


mkDefaultFile : Uuid -> RequestNode
mkDefaultFile id =
    RequestFile
        { id = id
        , name = NotEdited "new request"
        , httpUrl = NotEdited ""
        , httpMethod = NotEdited HttpGet
        , httpHeaders = NotEdited []
        , httpBody = NotEdited ""
        , showResponseView = False
        , whichResponseView = BodyResponseView
        , requestComputationResult = Nothing
        , runRequestIconAnimation = Animation.style []
        }



-- * view


view : Model a -> Element Msg
view model =
    let
        (RequestCollection _ requestNodes) =
            model.requestCollection

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
            column [ spacing 10 ] (nodeView model requestNodes)
    in
    column [ alignTop, spacing 20, centerX ]
        [ mainMenuView
        , treeView
        ]


nodeView : Model a -> List RequestNode -> List (Element Msg)
nodeView model requestCollection =
    case requestCollection of
        [] ->
            []

        node :: tail ->
            case node of
                RequestFolder { id, name, open, children } ->
                    let
                        folderChildrenView =
                            nodeView model children

                        tailView =
                            nodeView model tail

                        currentFolderView =
                            folderView id model name folderChildrenView open
                    in
                    currentFolderView :: tailView

                RequestFile { id, name } ->
                    let
                        tailView =
                            nodeView model tail

                        currentFileView =
                            fileView id model name
                    in
                    currentFileView :: tailView



-- ** file view


fileReadView : Model a -> String -> Uuid -> Element Msg
fileReadView model name id =
    let
        selected =
            model.displayedRequestId == Just id

        color =
            case selected of
                True -> primaryColor
                False -> secondaryColor

        weight =
            case selected of
               True -> Font.heavy
               False -> Font.regular
    in
    link [ weight ]
        { url = href (ReqPage (Just id) Nothing)
        , label = el [] <| iconWithTextAndColor "label" name color
        }

fileEditView : String -> Uuid -> Element Msg
fileEditView name id =
    Input.text
        [ Util.onEnterWithInput (AskRename id)
        ]
        { onChange = ChangeName id
        , text = name
        , placeholder = Nothing
        , label = Input.labelHidden "rename file"
        }

fileView : Uuid -> Model a -> Editable String -> Element Msg
fileView id model name =
    let
        modeView =
            case name of
                NotEdited value ->
                    fileReadView model value id

                Edited oldValue newValue ->
                    fileEditView newValue id

        showMenu =
            model.displayedRequestNodeMenuId == Just id

        menuView =
            case not showMenu of
                True ->
                    none

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
                        True ->
                            "more_horiz"

                        False ->
                            "more_vert"
            }
        , menuView
        ]



-- ** folder view


folderWithIconView : String -> Bool -> Element Msg
folderWithIconView name isOpen =
    let
        folderIconText =
            case isOpen of
                False ->
                    "keyboard_arrow_right"

                True ->
                    "keyboard_arrow_down"
    in
    iconWithText folderIconText name


folderMenuView : Uuid -> Bool -> Element Msg
folderMenuView id isOpen =
    let
        iconClass =
            case isOpen of
                True ->
                    "more_horiz"

                False ->
                    "more_vert"

        menuIcon =
            icon iconClass

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
        True ->
            row [] [ menuIcon, menuView ]

        False ->
            row [] [ menuIcon ]


folderReadView : Uuid -> String -> Bool -> Element Msg
folderReadView id name isOpen =
    Input.button []
        { onPress = Just <| ToggleFolder id
        , label = folderWithIconView name isOpen
        }


folderEditView : Uuid -> String -> Element Msg
folderEditView id name =
    Input.text
        [ Util.onEnterWithInput (AskRename id)
        ]
        { onChange = ChangeName id
        , text = name
        , placeholder = Nothing
        , label = Input.labelHidden "rename folder"
        }


folderView : Uuid -> Model a -> Editable String -> List (Element Msg) -> Bool -> Element Msg
folderView id model name folderChildrenView open =
    let
        modeView =
            case name of
                NotEdited value ->
                    folderReadView id value open

                Edited oldValue newValue ->
                    folderEditView id newValue

        showMenu =
            Just id == model.displayedRequestNodeMenuId
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
            True ->
                column [ spacing 10, paddingXY 20 10 ] folderChildrenView

            False ->
                none
        ]
