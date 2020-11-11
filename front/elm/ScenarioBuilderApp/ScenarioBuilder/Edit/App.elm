module ScenarioBuilderApp.ScenarioBuilder.Edit.App exposing (..)

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
import BuilderUtil exposing(..)
import ScenarioBuilderApp.ScenarioTree.App as Tree
import BuilderUtil exposing(..)
import Browser.Navigation as Navigation


-- * model


type alias Model a =
    { a
        | notification : Maybe Notification
        , scenarioCollection : ScenarioCollection
        , scenarioNewNode : NewNode
        , displayedScenarioBuilderView : RichBuilderView Uuid SceneDetailView
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
    | GenerateRandomUUIDForDuplicate (FileRecord ScenarioFileRecord)
    | AskDuplicate (FileRecord ScenarioFileRecord) Uuid
    | Duplicate (FileRecord ScenarioFileRecord) (Maybe Uuid)
    -- other
    | PrintNotification Notification


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
        SelectFolder id ->
            let
                oldLandingScenarioNewFolder =
                    model.scenarioNewNode

                newLandingScenarioNewFolder =
                    { oldLandingScenarioNewFolder | parentFolderId = Just id }

                newModel =
                    { model | scenarioNewNode = newLandingScenarioNewFolder }
            in
            (newModel, Cmd.none)

        SelectRootFolder ->
            let
                oldLandingScenarioNewFolder =
                    model.scenarioNewNode

                newLandingScenarioNewFolder =
                    { oldLandingScenarioNewFolder | parentFolderId = Nothing }

                newModel =
                    { model | scenarioNewNode = newLandingScenarioNewFolder }
            in
            (newModel, Cmd.none)

        UpdateName id newName ->
            let
                (ScenarioCollection scenarioCollectionId scenarioNodes) =
                    model.scenarioCollection

                newScenarioNodes =
                    List.map (modifyNode id (tempRename newName)) scenarioNodes

                newModel =
                    { model
                        | scenarioCollection =
                            ScenarioCollection scenarioCollectionId newScenarioNodes
                    }
            in
            ( newModel, Cmd.none )

        AskRename id newName ->
            let
                (ScenarioCollection scenarioCollectionId _) =
                    model.scenarioCollection

                payload =
                    Client.UpdateScenarioNode { updateScenarioNodeName = newName }

                newMsg =
                    Client.putApiScenarioCollectionByScenarioCollectionIdScenarioNodeByScenarioNodeId "" "" (Id scenarioCollectionId) (Id id) payload (renameNodeResultToMsg id newName)
            in
            ( model, newMsg )

        Rename id newName ->
            let
                (ScenarioCollection scenarioCollectionId scenarioNodes) =
                    model.scenarioCollection

                newScenarioNodes =
                    List.map (modifyNode id (rename newName)) scenarioNodes

                newModel =
                    { model
                        | scenarioCollection =
                            ScenarioCollection scenarioCollectionId newScenarioNodes
                    }
            in
            ( newModel, Cmd.none )

        AskDelete id ->
            let
                (ScenarioCollection scenarioCollectionId _) =
                    model.scenarioCollection

                newMsg =
                    Client.deleteApiScenarioCollectionByScenarioCollectionIdScenarioNodeByScenarioNodeId "" "" (Id scenarioCollectionId) (Id id) (deleteScenarioNodeResultToMsg id)
            in
            ( model, newMsg )

        Delete id ->
            let
                (ScenarioCollection scenarioCollectionId scenarioNodes) =
                    model.scenarioCollection

                newScenarioNodes =
                    List.concatMap (deleteNode id) scenarioNodes

                newModel =
                    { model
                        | scenarioCollection =
                            ScenarioCollection scenarioCollectionId newScenarioNodes
                    }

                newMsg =
                    Navigation.pushUrl model.navigationKey (href (ScenarioPage (RichLandingView DefaultView)))

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
                newModel =
                    { model | notification = Just (WarningNotification "This feature isn't implemented yet, sorry :-S" "") }
            in
            ( model, Cmd.none )

        Duplicate newFile mParentId ->
            let
                newModel =
                    { model | notification = Just (WarningNotification "This feature isn't implemented yet, sorry :-S" "") }
            in
            ( newModel, Cmd.none )

        PrintNotification notification ->
            ( { model | notification = Just notification }, Cmd.none )


-- * util


duplicateScenarioFileResultToMsg : FileRecord ScenarioFileRecord -> Maybe Uuid -> Result Http.Error () -> Msg
duplicateScenarioFileResultToMsg newFile mParentId result =
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

deleteScenarioNodeResultToMsg : Uuid -> Result Http.Error () -> Msg
deleteScenarioNodeResultToMsg id result =
    case result of
        Ok _ ->
            Delete id

        Err error ->
            PrintNotification <| AlertNotification "Could not delete, maybe this HTTP scenario is used in a scenario? Check the scenario and try reloading the page!" (httpErrorToString error)


-- * view


view : WhichEditView ScenarioNode -> Model a -> Element Msg
view whichEditView model =
    el ( box [ centerX, spacing 20, padding 30 ] ) <|
        case whichEditView of
            DefaultEditView scenarioNode ->
                defaultEditView model scenarioNode

            DeleteView scenarioNode ->
                deleteView model scenarioNode

            DuplicateView node ->
                case node of
                    Folder _ -> none
                    File fileRecord ->
                        duplicateView model fileRecord


-- ** default view


defaultEditView : Model a -> ScenarioNode -> Element Msg
defaultEditView model scenarioNode =
    let
        { id, name } =
            getNodeIdAndName scenarioNode

        nodeType =
            case scenarioNode of
                Folder _ -> "folder"
                File _ -> "file"

        label =
            case scenarioNode of
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
                , url = href (ScenarioPage (RichEditView (DeleteView id)))
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


deleteView : Model a -> ScenarioNode -> Element Msg
deleteView model scenarioNode =
    let
        { id, name } =
            getNodeIdAndName scenarioNode

        areYouSure =
            text "Are you sure you want to delete this?"

        yesBtn =
            Input.button primaryButtonAttrs
                { onPress = Just <| AskDelete id
                , label = text "Yes"
                }

        noBtn =
            link primaryButtonAttrs
                { url = href (ScenarioPage (RichEditView (DefaultEditView id)))
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


duplicateView : Model a -> FileRecord ScenarioFileRecord -> Element Msg
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

        (ScenarioCollection _ nodes) =
            model.scenarioCollection
    in
    column [ spacing 20 ]
        [ row [ width fill, centerY ]
              [ el [ alignLeft ] title
              , el [ alignRight ] (closeBuilderView model.page)
              ]
        , scenarioFolderTreeView nodes model.scenarioNewNode.parentFolderId SelectRootFolder SelectFolder
        , row [ centerX, spacing 20 ] [ duplicateBtn ]
        ]
