module RequestBuilderApp.RequestBuilder.Edit.App exposing (..)

import Api.Converter as Client
import Random
import Api.WebGeneratedClient as Client
import Api.RunnerGeneratedClient as Client
import Application.Type exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html as Html
import Html.Attributes as Html
import Html.Events as Html
import Http
import Util exposing (..)
import Uuid exposing (Uuid)
import Page exposing(..)
import HttpError exposing(..)
import RequestBuilderApp.RequestTree.Util as RequestTree
import RequestBuilderApp.RequestTree.App as RequestTree
import BuilderUtil exposing(..)


-- * model


type alias Model a =
    { a
        | notification : Maybe Notification
        , requestCollection : RequestCollection
        , requestNewNode : NewNode
        , displayedRequestBuilderView : BuilderView Uuid
    }


-- * msg


type Msg
    -- mkdir
    = SelectFolder Uuid
    | UpdateFolderName String
    | GenerateRandomUUIDForFolder Uuid
    | AskMkdir Uuid Uuid
    | Mkdir Uuid Uuid
    | NoOp
    -- touch
    | UpdateFileName String
    | GenerateRandomUUIDForFile Uuid
    | AskTouch Uuid Uuid
    | Touch Uuid Uuid
    -- other
    | PrintNotification Notification
    -- renaming
    | UpdateName Uuid String -- while focus is on the input
    | AskRename Uuid String
    | Rename Uuid String


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
        UpdateFolderName newName ->
            let
                oldLandingRequestNewFolder =
                    model.requestNewNode

                newLandingRequestNewFolder =
                    { oldLandingRequestNewFolder | name = newName }

                newModel =
                    { model | requestNewNode = newLandingRequestNewFolder }
            in
            (newModel, Cmd.none)

        SelectFolder id ->
            let
                oldLandingRequestNewFolder =
                    model.requestNewNode

                newLandingRequestNewFolder =
                    { oldLandingRequestNewFolder | parentFolderId = Just id }

                newModel =
                    { model | requestNewNode = newLandingRequestNewFolder }
            in
            (newModel, Cmd.none)

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
                    , newRequestFolderName = model.requestNewNode.name
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
                    List.map (RequestTree.modifyRequestNode parentNodeId (RequestTree.mkdir newId)) requestNodes

                newModel =
                    { model
                        | requestCollection =
                            RequestCollection id newRequestNodes
                    }
            in
            ( newModel, Cmd.none )

        UpdateFileName newName ->
            let
                oldLandingRequestNewFile =
                    model.requestNewNode

                newLandingRequestNewFile =
                    { oldLandingRequestNewFile | name = newName }

                newModel =
                    { model | requestNewNode = newLandingRequestNewFile }
            in
            (newModel, Cmd.none)

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

                    {-
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
                    List.map (RequestTree.modifyRequestNode parentNodeId (RequestTree.touch newId)) requestNodes

                newModel =
                    { model
                        | requestCollection =
                            RequestCollection id newRequestNodes
                    }
            in
            ( newModel, Cmd.none )

        NoOp ->
            (model, Cmd.none)

        UpdateName id newName ->
            let
                (RequestCollection requestCollectionId requestNodes) =
                    model.requestCollection

                newRequestNodes =
                    List.map (RequestTree.modifyRequestNode id (RequestTree.tempRename newName)) requestNodes

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
                    List.map (RequestTree.modifyRequestNode id (RequestTree.rename newName)) requestNodes

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


createRequestFolderResultToMsg : Uuid -> Uuid -> Result Http.Error () -> Msg
createRequestFolderResultToMsg parentNodeId id result =
    case result of
        Ok _ ->
            Mkdir parentNodeId id

        Err error ->
            PrintNotification <| AlertNotification "Could not create folder, try reloading the page!" (httpErrorToString error)

newRequestFileResultToMsg : Uuid -> Uuid -> Result Http.Error () -> Msg
newRequestFileResultToMsg parentNodeId id result =
    case result of
        Ok _ ->
            Touch parentNodeId id

        Err error ->
            PrintNotification <| AlertNotification "Could create a new file, try reloading the page" (httpErrorToString error)

renameNodeResultToMsg : Uuid -> String -> Result Http.Error () -> Msg
renameNodeResultToMsg id newName result =
    case result of
        Ok _ ->
            Rename id newName

        Err error ->
            PrintNotification <| AlertNotification "Could not rename, try reloading the page!" (httpErrorToString error)

-- * view


view : WhichEditView RequestNode -> Model a -> Element Msg
view whichEditView model =
    case whichEditView of
        DefaultEditView requestNode ->
            defaultEditView requestNode

        DuplicateView requestNode ->
            none


-- ** default view


defaultEditView : RequestNode -> Element Msg
defaultEditView requestNode =
    let
        { id, name } =
            getRequestNodeIdAndName requestNode

        placeholder =
            case requestNode of
                Folder _ -> "folder"
                File _ -> "file"

        label =
            case requestNode of
                Folder _ -> "Folder name:"
                File _ -> "Request name:"

        renameBtn =
            Input.button []
                { onPress = Just <| AskRename id (editedOrNotEditedValue name)
                , label = iconWithText "create_new_folder" "Rename"
                }

        renameInput =
            Input.text []
                  { onChange = UpdateName id
                  , text = editedOrNotEditedValue name
                  , placeholder = Just <| Input.placeholder [] (text placeholder)
                  , label = Input.labelLeft [ centerY ] <| text label
                  }

    in
    el [ alignTop, centerX, padding 20 ]
        <| column [ spacing 20 ]
            [ row [ spacing 20 ]
                  [ renameInput
                  , renameBtn
                  , closeBuilderView
                  ]
            , row [ spacing 20 ]
                [ text "Duplicate"
                ]
            ]



-- ** create folder view


createFolderView : Model a -> RequestNode -> Element Msg
createFolderView model requestNode =
    let

        createFolderButton =
            case model.requestNewNode.parentFolderId of
                Nothing ->
                    none

                Just parentId ->
                    Input.button []
                        { onPress = Just (GenerateRandomUUIDForFolder parentId)
                        , label = iconWithText "create_new_folder" "new folder"
                        }

        folderNameInput =
            Input.text []
                  { onChange = UpdateFolderName
                  , text = model.requestNewNode.name
                  , placeholder = Just <| Input.placeholder [] (text "myApi.com/path?arg=someArg")
                  , label = Input.labelLeft [ centerY ] <| text "Folder name: "
                  }
    in
    column []
        [ row []
              [ createFolderButton
              , closeBuilderView
              ]
        , folderNameInput
        , folderTreeView model SelectFolder
        ]


-- ** create default file view


createDefaultFileView : Model a -> Element Msg
createDefaultFileView model =
    let
        createFolderButton =
            case model.requestNewNode.parentFolderId of
                Nothing ->
                    none

                Just parentId ->
                    Input.button []
                        { onPress = Just (GenerateRandomUUIDForFile parentId)
                        , label = iconWithText "create_new_file" "new file"
                        }

        folderNameInput =
            Input.text []
                  { onChange = UpdateFileName
                  , text = model.requestNewNode.name
                  , placeholder = Just <| Input.placeholder [] (text "myApi.com/path?arg=someArg")
                  , label = Input.labelLeft [ centerY ] <| text "Folder name: "
                  }
    in
    column []
        [ row []
              [ createFolderButton
              , closeBuilderView
              ]
        , folderNameInput
        , folderTreeView model SelectFolder
        ]


-- ** folder tree view


folderTreeView : Model a -> (Uuid -> msg) -> Element msg
folderTreeView model selectFolderMsg =
    let
        (RequestCollection _ requestNodes) =
            model.requestCollection

        treeView =
            column [ spacing 10 ] (nodeView model selectFolderMsg requestNodes)
    in
    treeView


nodeView : Model a -> (Uuid -> msg) -> List RequestNode -> List (Element msg)
nodeView model selectFolderMsg requestCollection =
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
                            nodeView model selectFolderMsg c

                        tailView =
                            nodeView model selectFolderMsg tail

                        currentFolderView =
                            folderView id model name selectFolderMsg folderChildrenView
                    in
                    currentFolderView :: tailView

                File { id, name } ->
                    []


-- ** folder view


folderView : Uuid -> Model a -> Editable String -> (Uuid -> msg) -> List (Element msg) -> Element msg
folderView id model eName selectFolderMsg folderChildrenView =
    let
        selectFolderBtn : Element msg
        selectFolderBtn =
            Input.button []
                { onPress = Just (selectFolderMsg id)
                , label = text (editedOrNotEditedValue eName)
                }
    in
    column [ width (fill |> maximum 300) ]
        [ selectFolderBtn
        , column [ spacing 10, paddingXY 20 10 ] folderChildrenView
        ]
