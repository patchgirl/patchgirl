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
import Html as Html
import Html.Attributes as Html
import Html.Events as Html
import Http
import Util exposing (..)
import Uuid exposing (Uuid)
import Page exposing(..)
import HttpError exposing(..)
import RequestBuilderApp.RequestTree.Util as Tree
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
    -- rename
    | UpdateName Uuid String -- while focus is on the input
    | AskRename Uuid String
    | Rename Uuid String
    -- delete
    | AskDelete Uuid
    | Delete Uuid


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
        UpdateFolderName newName ->
            let
                oldLandingPgNewFolder =
                    model.pgNewNode

                newLandingPgNewFolder =
                    { oldLandingPgNewFolder | name = newName }

                newModel =
                    { model | pgNewNode = newLandingPgNewFolder }
            in
            (newModel, Cmd.none)

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

        GenerateRandomUUIDForFolder parentNodeId ->
            let
                newMsg =
                    Random.generate (AskMkdir parentNodeId) Uuid.uuidGenerator
            in
            ( model, newMsg )

        AskMkdir parentNodeId newId ->
            let
                (PgCollection pgCollectionId pgNodes) =
                    model.pgCollection

                newPgFolder =
                    { newPgFolderId = newId
                    , newPgFolderParentNodeId = parentNodeId
                    , newPgFolderName = model.pgNewNode.name
                    }

                newMsg =
                    Client.postApiPgCollectionByPgCollectionIdPgFolder "" "" pgCollectionId newPgFolder (createPgFolderResultToMsg parentNodeId newId)
            in
            ( model, newMsg )

        Mkdir parentNodeId newId ->
            let
                (PgCollection id pgNodes) =
                    model.pgCollection

                newPgNodes =
                    List.map (Tree.modifyPgNode parentNodeId (Tree.mkdirPg newId)) pgNodes

                newModel =
                    { model
                        | pgCollection =
                            PgCollection id newPgNodes
                    }
            in
            ( newModel, Cmd.none )

        UpdateFileName newName ->
            let
                oldLandingPgNewFile =
                    model.pgNewNode

                newLandingPgNewFile =
                    { oldLandingPgNewFile | name = newName }

                newModel =
                    { model | pgNewNode = newLandingPgNewFile }
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
                (PgCollection pgCollectionId pgNodes) =
                    model.pgCollection

                newPgFile =
                    { newPgFileId = newId
                    , newPgFileParentNodeId = parentNodeId
                    }

                    {-
                newMsg =
                    Client.postApiPgCollectionByPgCollectionIdPgFile "" "" pgCollectionId newPgFile (newPgFileResultToMsg parentNodeId newId)
                    -}
                newMsg =
                    Cmd.none
            in
            ( model, newMsg )

        Touch parentNodeId newId ->
            let
                (PgCollection id pgNodes) =
                    model.pgCollection

                newPgNodes =
                    List.map (Tree.modifyPgNode parentNodeId (Tree.touchPg newId)) pgNodes

                newModel =
                    { model
                        | pgCollection =
                            PgCollection id newPgNodes
                    }
            in
            ( newModel, Cmd.none )

        NoOp ->
            (model, Cmd.none)

        UpdateName id newName ->
            let
                (PgCollection pgCollectionId pgNodes) =
                    model.pgCollection

                newPgNodes =
                    List.map (Tree.modifyPgNode id (Tree.tempRenamePg newName)) pgNodes

                newModel =
                    { model
                        | pgCollection =
                            PgCollection pgCollectionId newPgNodes
                    }
            in
            ( newModel, Cmd.none )

        AskRename id newName ->
            let
                (PgCollection pgCollectionId pgNodes) =
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
                    List.map (Tree.modifyPgNode id (Tree.renamePg newName)) pgNodes

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
                    List.concatMap (Tree.deletePgNode id) pgNodes

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


createPgFolderResultToMsg : Uuid -> Uuid -> Result Http.Error () -> Msg
createPgFolderResultToMsg parentNodeId id result =
    case result of
        Ok _ ->
            Mkdir parentNodeId id

        Err error ->
            PrintNotification <| AlertNotification "Could not create folder, try reloading the page!" (httpErrorToString error)

newPgFileResultToMsg : Uuid -> Uuid -> Result Http.Error () -> Msg
newPgFileResultToMsg parentNodeId id result =
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

deletePgNodeResultToMsg : Uuid -> Result Http.Error () -> Msg
deletePgNodeResultToMsg id result =
    case result of
        Ok _ ->
            Delete id

        Err error ->
            PrintNotification <| AlertNotification "Could not delete, maybe this HTTP pg is used in a scenario? Check the scenario and try reloading the page!" (httpErrorToString error)


-- * view


view : WhichEditView PgNode -> Model a -> Element Msg
view whichEditView model =
    el ( box [ centerX, spacing 20, padding 30 ] ) <|
        case whichEditView of
            DefaultEditView pgNode ->
                defaultEditView model pgNode

            DeleteView pgNode ->
                deleteView model pgNode


-- ** default view


defaultEditView : Model a -> PgNode -> Element Msg
defaultEditView model pgNode =
    let
        { id, name } =
            getPgNodeIdAndName pgNode

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
        , row [ centerX ] [ deleteBtn ]
        ]


-- ** delete view


deleteView : Model a -> PgNode -> Element Msg
deleteView model pgNode =
    let
        { id, name } =
            getPgNodeIdAndName pgNode

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
