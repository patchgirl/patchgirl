module PGBuilderApp.PGBuilder.Edit.App exposing (..)

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
import Html
import Html.Attributes as Html
import Html.Events as Html
import Http
import Util exposing (..)
import Uuid exposing (Uuid)
import Page exposing(..)
import HttpError exposing(..)
import BuilderUtil as Tree
import PGBuilderApp.PGTree.App as Tree
import BuilderUtil exposing(..)
import Browser.Navigation as Navigation


-- * model


type alias Model a =
    { a
        | notification : Maybe Notification
        , pgCollection : PgCollection
        , pgNewNode : NewNode
        , displayedPgBuilderView : BuilderView Uuid
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
    | GenerateRandomUUIDForDuplicate (NodeRecord PgFileRecord)
    | AskDuplicate (NodeRecord PgFileRecord) Uuid
    | Duplicate (NodeRecord PgFileRecord) (Maybe Uuid)
    -- other
    | PrintNotification Notification


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
        SelectFolder id ->
            let
                oldLandingPgNewFolder =
                    model.pgNewNode

                newLandingPgNewFolder =
                    { oldLandingPgNewFolder | parentFolderId = Just id }

                newModel =
                    { model | pgNewNode = newLandingPgNewFolder }
            in
            (newModel, Cmd.none)

        SelectRootFolder ->
            let
                oldLandingPgNewFolder =
                    model.pgNewNode

                newLandingPgNewFolder =
                    { oldLandingPgNewFolder | parentFolderId = Nothing }

                newModel =
                    { model | pgNewNode = newLandingPgNewFolder }
            in
            (newModel, Cmd.none)

        UpdateName id newName ->
            let
                (PgCollection pgCollectionId pgNodes) =
                    model.pgCollection

                newPgNodes =
                    List.map (modifyNode id (Tree.tempRename newName)) pgNodes

                newModel =
                    { model
                        | pgCollection =
                            PgCollection pgCollectionId newPgNodes
                    }
            in
            ( newModel, Cmd.none )

        AskRename id newName ->
            let
                (PgCollection pgCollectionId _) =
                    model.pgCollection

                payload =
                    Client.UpdatePgNode { updatePgNodeName = newName }

                newMsg =
                    Client.putApiPgCollectionByPgCollectionIdPgNodeByPgNodeId "" "" pgCollectionId id payload (renameNodeResultToMsg id newName)
            in
            ( model, newMsg )

        Rename id newName ->
            let
                (PgCollection pgCollectionId pgNodes) =
                    model.pgCollection

                newPgNodes =
                    List.map (modifyNode id (Tree.rename newName)) pgNodes

                newModel =
                    { model
                        | pgCollection =
                            PgCollection pgCollectionId newPgNodes
                    }
            in
            ( newModel, Cmd.none )

        AskDelete id ->
            let
                (PgCollection pgCollectionId _) =
                    model.pgCollection

                newMsg =
                    Client.deleteApiPgCollectionByPgCollectionIdPgNodeByPgNodeId "" "" pgCollectionId id (deletePgNodeResultToMsg id)
            in
            ( model, newMsg )

        Delete id ->
            let
                (PgCollection pgCollectionId pgNodes) =
                    model.pgCollection

                newPgNodes =
                    List.concatMap (deleteNode id) pgNodes

                newModel =
                    { model
                        | pgCollection =
                            PgCollection pgCollectionId newPgNodes
                    }

                newMsg =
                    Navigation.pushUrl model.navigationKey (href (PgPage (LandingView DefaultView)))

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
                (PgCollection pgCollectionId _) =
                    model.pgCollection

                newFileRecord =
                    { origFileRecord
                        | id = newId
                        , name = NotEdited <| (notEditedValue origFileRecord.name) ++ " copy"
                    }

                newMsg =
                    case model.pgNewNode.parentFolderId of
                        Nothing ->
                            let
                                payload =
                                    { newRootPgFileId = newFileRecord.id
                                    , newRootPgFileName = editedOrNotEditedValue newFileRecord.name
                                    , newRootPgFileSql = editedOrNotEditedValue newFileRecord.sql
                                    , newRootPgFileHost = editedOrNotEditedValue newFileRecord.dbHost
                                    , newRootPgFilePassword = editedOrNotEditedValue newFileRecord.dbPassword
                                    , newRootPgFilePort = editedOrNotEditedValue newFileRecord.dbPort
                                    , newRootPgFileUser = editedOrNotEditedValue newFileRecord.dbUser
                                    , newRootPgFileDbName = editedOrNotEditedValue newFileRecord.dbName
                                    }
                            in
                            Client.postApiPgCollectionByPgCollectionIdRootPgFile "" "" pgCollectionId payload (duplicatePgFileResultToMsg newFileRecord model.pgNewNode.parentFolderId)

                        Just folderId ->
                            let
                                payload =
                                    { newPgFileId = newFileRecord.id
                                    , newPgFileParentNodeId = folderId
                                    , newPgFileName = editedOrNotEditedValue newFileRecord.name
                                    , newPgFileSql = editedOrNotEditedValue newFileRecord.sql
                                    , newPgFileHost = editedOrNotEditedValue newFileRecord.dbHost
                                    , newPgFilePassword = editedOrNotEditedValue newFileRecord.dbPassword
                                    , newPgFilePort = editedOrNotEditedValue newFileRecord.dbPort
                                    , newPgFileUser = editedOrNotEditedValue newFileRecord.dbUser
                                    , newPgFileDbName = editedOrNotEditedValue newFileRecord.dbName
                                    }
                            in
                            Client.postApiPgCollectionByPgCollectionIdPgFile "" "" pgCollectionId payload (duplicatePgFileResultToMsg newFileRecord model.pgNewNode.parentFolderId)

            in
            ( model, newMsg )

        Duplicate newFile mParentId ->
            let
                (PgCollection pgCollectionId pgNodes) =
                    model.pgCollection

                newPgNodes =
                    case mParentId of
                        Nothing ->
                            pgNodes ++ [ File newFile ]

                        Just folderId ->
                            List.map (modifyNode folderId (touchNode (File newFile))) pgNodes

                newModel =
                    { model
                        | pgCollection =
                            PgCollection pgCollectionId newPgNodes
                    }

                newMsg =
                    Navigation.pushUrl model.navigationKey (href (PgPage (LandingView DefaultView)))

            in
            ( newModel, newMsg )

        PrintNotification notification ->
            ( { model | notification = Just notification }, Cmd.none )


-- * util


renameNodeResultToMsg : Uuid -> String -> Result Http.Error () -> Msg
renameNodeResultToMsg id newName result =
    case result of
        Ok _ ->
            Rename id newName

        Err error ->
            PrintNotification <| AlertNotification "Could not rename, try reloading the page!" (httpErrorToString error)

deletePgNodeResultToMsg : Uuid -> Result Http.Error () -> Msg
deletePgNodeResultToMsg id result =
    case result of
        Ok _ ->
            Delete id

        Err error ->
            PrintNotification <| AlertNotification "Could not delete, maybe this HTTP pg is used in a scenario? Check the scenario and try reloading the page!" (httpErrorToString error)

duplicatePgFileResultToMsg : NodeRecord PgFileRecord -> Maybe Uuid -> Result Http.Error () -> Msg
duplicatePgFileResultToMsg newFile mParentId result =
    case result of
        Ok _ ->
            Duplicate newFile mParentId

        Err error ->
            PrintNotification <| AlertNotification "Could not duplicate file, try reloading the page" (httpErrorToString error)

-- * view


view : WhichEditView PgNode -> Model a -> Element Msg
view whichEditView model =
    el ( box [ centerX, spacing 20, padding 30 ] ) <|
        case whichEditView of
            DefaultEditView pgNode ->
                defaultEditView model pgNode

            DeleteView pgNode ->
                deleteView model pgNode

            DuplicateView pgNode ->
                case pgNode of
                    Folder _ -> none
                    File fileRecord ->
                        duplicateView model fileRecord


-- ** default view


defaultEditView : Model a -> PgNode -> Element Msg
defaultEditView model pgNode =
    let
        { id, name } =
            getNodeIdAndName pgNode

        nodeType =
            case pgNode of
                Folder _ -> "folder"
                File _ -> "file"

        label =
            case pgNode of
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
                , url = href (PgPage (EditView (DeleteView id)))
                }

        duplicateBtn =
            link primaryButtonAttrs
                { label =
                      iconWithAttr { defaultIconAttribute
                                       | title = " Duplicate"
                                       , icon = "content_copy"
                                   }
                , url = href (PgPage (EditView (DuplicateView id)))
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
            [ case pgNode of
                  File _ -> duplicateBtn
                  _ -> none
            , deleteBtn
            ]
        ]


-- ** delete view


deleteView : Model a -> PgNode -> Element Msg
deleteView model pgNode =
    let
        { id, name } =
            getNodeIdAndName pgNode

        areYouSure =
            text "Are you sure you want to delete this?"

        yesBtn =
            Input.button primaryButtonAttrs
                { onPress = Just <| AskDelete id
                , label = text "Yes"
                }

        noBtn =
            link primaryButtonAttrs
                { url = href (PgPage (EditView (DefaultEditView id)))
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


duplicateView : Model a -> NodeRecord PgFileRecord -> Element Msg
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

        (PgCollection _ nodes) =
            model.pgCollection
    in
    column [ spacing 20 ]
        [ row [ width fill, centerY ]
              [ el [ alignLeft ] title
              , el [ alignRight ] (closeBuilderView model.page)
              ]
        , pgFolderTreeView nodes model.pgNewNode.parentFolderId SelectRootFolder SelectFolder
        , row [ centerX, spacing 20 ] [ duplicateBtn ]
        ]
