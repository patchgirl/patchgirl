module RequestBuilderApp.RequestBuilder.Edit.App exposing (..)

import Api.Converter as Client
import Random
import Api.WebGeneratedClient as Client exposing (Id(..))
import Api.RunnerGeneratedClient as Client
import Application.Type exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes as Html
import Html.Events as Html
import Http
import Util exposing (..)
import Uuid exposing (Uuid)
import Page exposing(..)
import HttpError exposing(..)
import BuilderUtil exposing (..)
import RequestBuilderApp.RequestTree.App as RequestTree
import BuilderUtil exposing(..)
import Browser.Navigation as Navigation


-- * model


type alias Model a =
    { a
        | notification : Maybe Notification
        , requestCollection : RequestCollection
        , requestNewNode : NewNode
        , displayedRequestBuilderView : BuilderView Uuid
        , navigationKey : Navigation.Key
        , page : Page
    }


-- * msg


type Msg
    = SelectFolder Uuid
    | SelectRootFolder
    -- rename
    | UpdateName Uuid String -- while focus is on the input
    | AskRename Uuid String
    | Rename Uuid String
    -- delete
    | AskDelete Uuid
    | Delete Uuid
    -- duplicate
    | GenerateRandomUUIDForDuplicate (NodeRecord RequestFileRecord)
    | AskDuplicate (NodeRecord RequestFileRecord) Uuid
    | Duplicate (NodeRecord RequestFileRecord) (Maybe Uuid)
    -- other
    | PrintNotification Notification


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
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

        SelectRootFolder ->
            let
                oldLandingRequestNewFolder =
                    model.requestNewNode

                newLandingRequestNewFolder =
                    { oldLandingRequestNewFolder | parentFolderId = Nothing }

                newModel =
                    { model | requestNewNode = newLandingRequestNewFolder }
            in
            (newModel, Cmd.none)

        UpdateName id newName ->
            let
                (RequestCollection requestCollectionId requestNodes) =
                    model.requestCollection

                newRequestNodes =
                    List.map (modifyNode id (tempRename newName)) requestNodes

                newModel =
                    { model
                        | requestCollection =
                            RequestCollection requestCollectionId newRequestNodes
                    }
            in
            ( newModel, Cmd.none )

        AskRename id newName ->
            let
                (RequestCollection requestCollectionId _) =
                    model.requestCollection

                payload =
                    Client.UpdateRequestNode { updateRequestNodeName = newName }

                newMsg =
                    Client.putApiRequestCollectionByRequestCollectionIdRequestNodeByRequestNodeId "" "" requestCollectionId (Client.Id id) payload (renameNodeResultToMsg id newName)
            in
            ( model, newMsg )

        Rename id newName ->
            let
                (RequestCollection requestCollectionId requestNodes) =
                    model.requestCollection

                newRequestNodes =
                    List.map (modifyNode id (rename newName)) requestNodes

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
                    Client.deleteApiRequestCollectionByRequestCollectionIdRequestNodeByRequestNodeId "" "" requestCollectionId (Client.Id id) (deleteRequestNodeResultToMsg id)
            in
            ( model, newMsg )

        Delete id ->
            let
                (RequestCollection requestCollectionId requestNodes) =
                    model.requestCollection

                newRequestNodes =
                    List.concatMap (deleteNode id) requestNodes

                newModel =
                    { model
                        | requestCollection =
                            RequestCollection requestCollectionId newRequestNodes
                    }

                newMsg =
                    Navigation.pushUrl model.navigationKey (href (ReqPage (LandingView DefaultView)))

            in
            ( newModel, newMsg )

        GenerateRandomUUIDForDuplicate origFileRecord ->
            let
                newMsg =
                    Random.generate (AskDuplicate origFileRecord) Uuid.uuidGenerator
            in
            ( model, newMsg )

        AskDuplicate origFileRecord newId ->
            let
                (RequestCollection requestCollectionId _) =
                    model.requestCollection

                newFileRecord =
                    { origFileRecord
                        | id = newId
                        , name = NotEdited <| (notEditedValue origFileRecord.name) ++ " copy"
                    }

                newMsg =
                    case model.requestNewNode.parentFolderId of
                        Nothing ->
                            let
                                payload =
                                       { newRootRequestFileId = Id newFileRecord.id
                                       , newRootRequestFileName = editedOrNotEditedValue newFileRecord.name
                                       , newRootRequestFileHttpUrl = editedOrNotEditedValue newFileRecord.httpUrl
                                       , newRootRequestFileMethod = editedOrNotEditedValue newFileRecord.httpMethod |> Client.convertMethodFromFrontToBack
                                       , newRootRequestFileHeaders = editedOrNotEditedValue newFileRecord.httpHeaders
                                       , newRootRequestFileBody = editedOrNotEditedValue newFileRecord.httpBody
                                       }
                            in
                            Client.postApiRequestCollectionByRequestCollectionIdRootRequestFile "" "" requestCollectionId payload (duplicateRequestFileResultToMsg newFileRecord model.requestNewNode.parentFolderId)

                        Just folderId ->
                            let
                                payload =
                                    { newRequestFileId = Id newFileRecord.id
                                    , newRequestFileParentNodeId = Id folderId
                                    , newRequestFileName = editedOrNotEditedValue newFileRecord.name
                                    , newRequestFileHttpUrl = editedOrNotEditedValue newFileRecord.httpUrl
                                    , newRequestFileMethod = editedOrNotEditedValue newFileRecord.httpMethod |> Client.convertMethodFromFrontToBack
                                    , newRequestFileHeaders = editedOrNotEditedValue newFileRecord.httpHeaders
                                    , newRequestFileBody = editedOrNotEditedValue newFileRecord.httpBody
                                    }
                            in
                            Client.postApiRequestCollectionByRequestCollectionIdRequestFile "" "" requestCollectionId payload (duplicateRequestFileResultToMsg newFileRecord model.requestNewNode.parentFolderId)
            in
            ( model, newMsg )

        Duplicate newFile mParentId ->
            let
                (RequestCollection requestCollectionId requestNodes) =
                    model.requestCollection

                newRequestNodes =
                    case mParentId of
                        Nothing ->
                            requestNodes ++ [ File newFile ]

                        Just folderId ->
                            List.map (modifyNode folderId (touchNode (File newFile))) requestNodes

                newModel =
                    { model
                        | requestCollection =
                            RequestCollection requestCollectionId newRequestNodes
                    }

                newMsg =
                    Navigation.pushUrl model.navigationKey (href (ReqPage (LandingView DefaultView)))

            in
            ( newModel, newMsg )

        PrintNotification notification ->
            ( { model | notification = Just notification }, Cmd.none )


-- * util


duplicateRequestFileResultToMsg : NodeRecord RequestFileRecord -> Maybe Uuid -> Result Http.Error () -> Msg
duplicateRequestFileResultToMsg newFile mParentId result =
    case result of
        Ok _ ->
            Duplicate newFile mParentId

        Err error ->
            PrintNotification <| AlertNotification "Could not duplicate file, try reloading the page" (httpErrorToString error)

renameNodeResultToMsg : Uuid -> String -> Result Http.Error () -> Msg
renameNodeResultToMsg id newName result =
    case result of
        Ok _ ->
            Rename id newName

        Err error ->
            PrintNotification <| AlertNotification "Could not rename, try reloading the page!" (httpErrorToString error)

deleteRequestNodeResultToMsg : Uuid -> Result Http.Error () -> Msg
deleteRequestNodeResultToMsg id result =
    case result of
        Ok _ ->
            Delete id

        Err error ->
            PrintNotification <| AlertNotification "Could not delete, maybe this HTTP request is used in a scenario? Check the scenario and try reloading the page!" (httpErrorToString error)


-- * view


view : WhichEditView RequestNode -> Model a -> Element Msg
view whichEditView model =
    el ( box [ centerX, spacing 20, padding 30 ] ) <|
        case whichEditView of
            DefaultEditView requestNode ->
                defaultEditView model requestNode

            DeleteView requestNode ->
                deleteView model requestNode

            DuplicateView requestNode ->
                case requestNode of
                    Folder _ -> none
                    File fileRecord ->
                        duplicateView model fileRecord


-- ** default view


defaultEditView : Model a -> RequestNode -> Element Msg
defaultEditView model requestNode =
    let
        { id, name } =
            getNodeIdAndName requestNode

        nodeType =
            case requestNode of
                Folder _ -> "folder"
                File _ -> "file"

        label =
            case requestNode of
                Folder _ -> "name: "
                File _ -> "name: "

        renameBtn =
            Input.button primaryButtonAttrs
                { onPress = Just <|
                      AskRename id (editedOrNotEditedValue name)
                , label =
                    iconWithAttr { defaultIconAttribute
                                     | title = " Save"
                                     , icon = "save"
                                 }
                }

        deleteBtn =
            link primaryButtonAttrs
                { label =
                      iconWithAttr { defaultIconAttribute
                                       | title = " Delete"
                                       , icon = "delete"
                                   }
                , url = href (ReqPage (EditView (DeleteView id)))
                }

        duplicateBtn =
            link primaryButtonAttrs
                { label =
                      iconWithAttr { defaultIconAttribute
                                       | title = " Duplicate"
                                       , icon = "content_copy"
                                   }
                , url = href (ReqPage (EditView (DuplicateView id)))
                }

        renameInput =
            Input.text []
                  { onChange = UpdateName id
                  , text = editedOrNotEditedValue name
                  , placeholder = Just <| Input.placeholder [] (text nodeType)
                  , label = Input.labelLeft [ centerY ] <| text label
                  }

        title =
            el [ Font.size 25, Font.underline ] (text ("Edit " ++ nodeType ++ ": " ++ (editedOrNotEditedValue name)))
    in
    column [ spacing 20 ]
        [ row [ width fill, centerY ]
              [ el [ alignLeft ] title
              , el [ alignRight ] (closeBuilderView model.page)
              ]
        , row [ spacing 20 ]
                  [ renameInput
                  , renameBtn
                  ]
        , el [ centerX ] (text "or ")
        , row [ centerX, spacing 10 ]
            [ case requestNode of
                  File _ -> duplicateBtn
                  _ -> none
            , deleteBtn
            ]
        ]


-- ** delete view


deleteView : Model a -> RequestNode -> Element Msg
deleteView model requestNode =
    let
        { id, name } =
            getNodeIdAndName requestNode

        areYouSure =
            text "Are you sure you want to delete this?"

        yesBtn =
            Input.button primaryButtonAttrs
                { onPress = Just <| AskDelete id
                , label = text "Yes"
                }

        noBtn =
            link primaryButtonAttrs
                { url = href (ReqPage (EditView (DefaultEditView id)))
                , label = text "No"
                }

        title =
            el [ Font.size 25, Font.underline ] <| text ("Delete " ++ (editedOrNotEditedValue name))
    in
    column [ spacing 20 ]
        [ row [ width fill, centerY ]
              [ el [ alignLeft ] title
              , el [ alignRight ] (closeBuilderView model.page)
              ]
        , areYouSure
        , row [ centerX, spacing 20 ] [ noBtn, yesBtn ]
        ]


-- ** duplicate view


duplicateView : Model a -> NodeRecord RequestFileRecord -> Element Msg
duplicateView model fileRecord =
    let
        name =
            editedOrNotEditedValue fileRecord.name

        duplicateBtn =
            Input.button primaryButtonAttrs
                { onPress = Just <| GenerateRandomUUIDForDuplicate fileRecord
                , label = text "Duplicate"
                }

        title =
            el [ Font.size 25, Font.underline ] <| text ("Duplicate " ++ name)

        (RequestCollection _ nodes) =
            model.requestCollection
    in
    column [ spacing 20 ]
        [ row [ width fill, centerY ]
              [ el [ alignLeft ] title
              , el [ alignRight ] (closeBuilderView model.page)
              ]
        , folderTreeView nodes model.requestNewNode.parentFolderId SelectRootFolder SelectFolder
        , row [ centerX, spacing 20 ] [ duplicateBtn ]
        ]
