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
import Element.Events exposing (..)
import RequestBuilderApp.RequestTree.Util exposing(..)


-- * model


type alias Model a =
    { a
        | requestCollection : RequestCollection
        , notification : Maybe Notification
        , displayedRequestNodeMenuId : Maybe Uuid
        , displayedRequestId : Maybe Uuid
        , displayedRequestBuilderView : BuilderView Uuid
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , navigationKey : Navigation.Key
    }


-- * message


type Msg
    = ToggleFolder Uuid
    | ToggleMenu (Maybe Uuid)
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
        ToggleMenu mId ->
            ({ model | displayedRequestNodeMenuId = mId }, Cmd.none)

{-        ToggleMenu id ->
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
            ( newModel, Cmd.none ) -}

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
                {-
                (RequestCollection requestCollectionId requestNodes) =
                    model.requestCollection

                newRequestFile =
                    { newRequestFileId = newId
                    , newRequestFileParentNodeId = parentNodeId
                    }

                newMsg =
                    Client.postApiRequestCollectionByRequestCollectionIdRequestFile "" "" requestCollectionId newRequestFile (newRequestFileResultToMsg parentNodeId newId)
                        -}
                newMsg =
                    Cmd.none
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
                    Cmd.none
                    {-
                    case model.displayedRequestId == (Just id, _) of
                        True ->
                            Navigation.pushUrl model.navigationKey (href (ReqPage Nothing Nothing))

                        False ->
                            Cmd.none
                            -}

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
                {-
                (RequestCollection requestCollectionId _) =
                    model.requestCollection

                newRootRequestFile =
                    Client.NewRootRequestFile { newRootRequestFileId = newId }

                newMsg =
                    Client.postApiRequestCollectionByRequestCollectionIdRootRequestFile "" "" requestCollectionId newRootRequestFile (createRootRequestFileResultToMsg newId)
                    -}
                newMsg = Cmd.none
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
{-                (RequestCollection requestCollectionId _) =
                    model.requestCollection

                newRootRequestFolder =
                    Client.NewRootRequestFolder { newRootRequestFolderId = newId }

                newMsg =
                    Client.postApiRequestCollectionByRequestCollectionIdRootRequestFolder "" "" requestCollectionId newRootRequestFolder (createRootRequestFolderResultToMsg newId)
                    -}
                newMsg = Cmd.none
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
                Folder { id, name, open, children } ->
                    let
                        (Children c) =
                            children

                        folderChildrenView =
                            nodeView model c

                        tailView =
                            nodeView model tail

                        currentFolderView =
                            folderView id model name folderChildrenView open
                    in
                    currentFolderView :: tailView

                File { id, name } ->
                    let
                        tailView =
                            nodeView model tail

                        currentFileView =
                            fileView id model name
                    in
                    currentFileView :: tailView



-- ** file view


fileView : Uuid -> Model a -> Editable String -> Element Msg
fileView id model eName =
    let
        showMenu =
            model.displayedRequestNodeMenuId == Just id

        folderMenuView : Element Msg
        folderMenuView =
            case showMenu of
                True ->
                    link []
                        { url = href (ReqPage (EditView (DefaultEditView id)))
                        , label = editIcon
                        }

                False ->
                    none

        name =
            editedOrNotEditedValue eName

        selected =
            False
            {-
            case model.displayedRequestBuilderView of
                Just (EditView i) ->
                    i == id
                Just (RunView i) ->
                    i == id
                _ ->
                    False
                    -}

        color =
            case selected of
                True -> primaryColor
                False -> secondaryColor

        weight =
            case selected of
               True -> Font.heavy
               False -> Font.regular
    in
    row [ onMouseEnter (ToggleMenu (Just id))
        , onMouseLeave (ToggleMenu Nothing)
        ]
        [ link [ weight ]
              { url = href (ReqPage (RunView id))
              , label = el [] <| iconWithTextAndColor "label" name color
              }
        , folderMenuView
        ]


-- ** folder view


folderView : Uuid -> Model a -> Editable String -> List (Element Msg) -> Bool -> Element Msg
folderView id model eName folderChildrenView open =
    let
        folderWithIconView : Element Msg
        folderWithIconView =
            let
                folderIconText =
                    case open of
                        False ->
                            "keyboard_arrow_right"

                        True ->
                            "keyboard_arrow_down"
            in
            iconWithText folderIconText name

        showMenu =
            Just id == model.displayedRequestNodeMenuId

        name =
            editedOrNotEditedValue eName

        selected =
                {-
            case model.displayedRequestBuilderView of
                Just (EditView i) ->
                    i == id
                Just (RunView i) ->
                    i == id
                _ ->-}
                    False

        color =
            case selected of
                True -> primaryColor
                False -> secondaryColor

        weight =
            case selected of
               True -> Font.heavy
               False -> Font.regular

        folderReadView : Bool -> Element Msg
        folderReadView isOpen =
            Input.button [ weight ]
                { onPress = Just <| ToggleFolder id
                , label = folderWithIconView
                }

        folderMenuView : Element Msg
        folderMenuView =
            case showMenu of
                True ->
                    link []
                        { url = href (ReqPage (EditView (DefaultEditView id)))
                        , label = editIcon
                        }

                False ->
                    none
    in
    column [ width (fill |> maximum 300) ]
        [ row [ onMouseEnter (ToggleMenu (Just id))
              , onMouseLeave (ToggleMenu Nothing)
              ]
              [ folderReadView open
              , folderMenuView
              ]
        , case open of
            True ->
                column [ spacing 10, paddingXY 20 10 ] folderChildrenView

            False ->
                none
        ]
