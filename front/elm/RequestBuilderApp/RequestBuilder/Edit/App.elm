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
    -- rename
    | UpdateName Uuid String -- while focus is on the input
    | AskRename Uuid String
    | Rename Uuid String
    -- delete
    | AskDelete Uuid
    | Delete Uuid
    -- duplicate
    | GenerateRandomUUIDForDuplicate Uuid
    | AskDuplicate Uuid Uuid
    | Duplicate Uuid Uuid (Maybe Uuid)
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

        UpdateName id newName ->
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
                (RequestCollection requestCollectionId _) =
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
                    Navigation.pushUrl model.navigationKey (href (ReqPage (LandingView DefaultView)))

            in
            ( newModel, newMsg )

        GenerateRandomUUIDForDuplicate origId ->
            let
                newMsg =
                    Random.generate (AskDuplicate origId) Uuid.uuidGenerator
            in
            ( model, newMsg )

        AskDuplicate origId newId ->
            let
                (RequestCollection requestCollectionId _) =
                    model.requestCollection

                nodeParentId =
                    model.requestNewNode.parentFolderId

                payload =
                    { duplicateNodeNewId = newId
                    , duplicateNodeTargetId = nodeParentId
                    }

                newMsg =
                    Client.postApiRequestCollectionByRequestCollectionIdRequestNodeByRequestNodeId "" "" requestCollectionId origId payload (duplicateRequestNodeResultToMsg origId newId nodeParentId)
            in
            ( model, newMsg )

        Duplicate origId newId nodeParentId ->
            let
                (RequestCollection requestCollectionId requestNodes) =
                    model.requestCollection

                newRequestNodes =
                    case nodeParentId of
                        Nothing ->
                            requestNodes ++ [ mkDefaultRequestFile newId ]

                        Just folderId ->
                            List.map (modifyRequestNode folderId (touchRequest newId)) requestNodes

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

duplicateRequestNodeResultToMsg : Uuid -> Uuid -> Maybe Uuid -> Result Http.Error () -> Msg
duplicateRequestNodeResultToMsg origId newId nodeParentId result =
    case result of
        Ok _ ->
            Duplicate origId newId nodeParentId

        Err error ->
            PrintNotification <| AlertNotification "Could not duplicate! Try reloading the page!" (httpErrorToString error)


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
                duplicateView model requestNode


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


duplicateView : Model a -> RequestNode -> Element Msg
duplicateView model requestNode =
    let
        { id, name } =
            getNodeIdAndName requestNode

        duplicateBtn =
            Input.button primaryButtonAttrs
                { onPress = Just <| GenerateRandomUUIDForDuplicate id
                , label = text "Duplicate"
                }

        title =
            el [ Font.size 25, Font.underline ] <| text ("Duplicate " ++ (editedOrNotEditedValue name))

        (RequestCollection _ nodes) =
            model.requestCollection
    in
    column [ spacing 20 ]
        [ row [ width fill, centerY ]
              [ el [ alignLeft ] title
              , el [ alignRight ] (closeBuilderView model.page)
              ]
        , folderTreeView nodes model.requestNewNode.parentFolderId SelectFolder
        , row [ centerX, spacing 20 ] [ duplicateBtn ]
        ]
