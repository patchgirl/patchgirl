module PGBuilderApp.PGBuilder.Landing.App exposing (..)

import Api.Converter as Client
import Random
import Api.WebGeneratedClient as Client exposing(Id(..))
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
import PGBuilderApp.PGTree.App as PgTree
import BuilderUtil exposing (..)
import Animation


-- * model


type alias Model a =
    { a
        | notification : Maybe Notification
        , pgCollection : PgCollection
        , pgNewNode : NewNode
        , page : Page
    }


-- * msg


type Msg
    = SelectFolder (Maybe Uuid)
    | ChangeName String
    -- mkdir
    | GenerateRandomUUIDForFolder NewNode
    | AskMkdir NewNode Uuid
    | Mkdir NewNode Uuid
    -- touch
    | GenerateRandomUUIDForFile NewNode
    | AskTouch NewNode Uuid
    | Touch NewNode Uuid
    -- other
    | PrintNotification Notification


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
        ChangeName newName ->
            let
                oldLandingPgNewFolder =
                    model.pgNewNode

                newLandingPgNewFolder =
                    { oldLandingPgNewFolder | name = newName }

                newModel =
                    { model | pgNewNode = newLandingPgNewFolder }
            in
            (newModel, Cmd.none)

        SelectFolder mId ->
            let
                oldLandingPgNewNode =
                    model.pgNewNode

                newLandingPgNewNode =
                    { oldLandingPgNewNode | parentFolderId = mId }

                newModel =
                    { model | pgNewNode = newLandingPgNewNode }
            in
            (newModel, Cmd.none)

        GenerateRandomUUIDForFolder newNode ->
            let
                newMsg =
                    Random.generate (AskMkdir newNode) Uuid.uuidGenerator
            in
            ( model, newMsg )

        AskMkdir newNode newId ->
            let
                (PgCollection pgCollectionId _) =
                    model.pgCollection

                newMsg =
                    case newNode.parentFolderId of
                        Nothing ->
                            let
                                payload =
                                    { newRootPgFolderId = (Id newId)
                                    , newRootPgFolderName = newNode.name
                                    }
                            in
                            Client.postApiPgCollectionByPgCollectionIdRootPgFolder "" "" (Id pgCollectionId) payload (createPgFolderResultToMsg newNode newId)

                        Just folderId ->
                            let
                                payload =
                                    { newPgFolderId = (Id newId)
                                    , newPgFolderParentNodeId = (Id folderId)
                                    , newPgFolderName = newNode.name
                                    }
                            in
                            Client.postApiPgCollectionByPgCollectionIdPgFolder "" "" (Id pgCollectionId) payload (createPgFolderResultToMsg newNode newId)

            in
            ( model, newMsg )

        Mkdir newNode newId ->
            let
                (PgCollection id pgNodes) =
                    model.pgCollection

                newFolder =
                    mkDefaultFolder newId newNode.name

                newPgNodes =
                    case newNode.parentFolderId of
                        Nothing ->
                            pgNodes ++ [ Folder newFolder ]

                        Just folderId ->
                            List.map (modifyNode folderId (mkdirNode newFolder)) pgNodes

                newModel =
                    { model
                        | pgCollection =
                            PgCollection id newPgNodes
                    }
            in
            ( newModel, Cmd.none )

        GenerateRandomUUIDForFile newNode ->
            let
                newMsg =
                    Random.generate (AskTouch newNode) Uuid.uuidGenerator
            in
            ( model, newMsg )

        AskTouch newNode newId ->
            let
                (PgCollection pgCollectionId _) =
                    model.pgCollection

                newMsg =
                    case newNode.parentFolderId of
                        Nothing ->
                            let
                                payload =
                                    { newRootPgFileId = (Id newId)
                                    , newRootPgFileName = newNode.name
                                    , newRootPgFileSql = ""
                                    , newRootPgFileHost = ""
                                    , newRootPgFilePassword = ""
                                    , newRootPgFilePort = ""
                                    , newRootPgFileUser = ""
                                    , newRootPgFileDbName = ""
                                    }
                            in
                            Client.postApiPgCollectionByPgCollectionIdRootPgFile "" "" (Id pgCollectionId) payload (createPgFileResultToMsg newNode newId)

                        Just folderId ->
                            let
                                payload =
                                    { newPgFileId = (Id newId)
                                    , newPgFileParentNodeId = (Id folderId)
                                    , newPgFileName = newNode.name
                                    , newPgFileSql = ""
                                    , newPgFileHost = ""
                                    , newPgFilePassword = ""
                                    , newPgFilePort = ""
                                    , newPgFileUser = ""
                                    , newPgFileDbName = ""
                                    }
                            in
                            Client.postApiPgCollectionByPgCollectionIdPgFile "" "" (Id pgCollectionId) payload (createPgFileResultToMsg newNode newId)

            in
            ( model, newMsg )

        Touch newNode newId ->
            let
                (PgCollection id pgNodes) =
                    model.pgCollection

                newPgNode =
                    File (mkDefaultPgFile newId newNode.name)

                newPgNodes =
                    case newNode.parentFolderId of
                        Nothing ->
                            pgNodes ++ [ newPgNode ]

                        Just folderId ->
                            List.map (modifyNode folderId (touchNode newPgNode)) pgNodes

                newModel =
                    { model
                        | pgCollection =
                            PgCollection id newPgNodes
                    }
            in
            ( newModel, Cmd.none )

        PrintNotification notification ->
            ( { model | notification = Just notification }, Cmd.none )


-- * util


createPgFileResultToMsg : NewNode -> Uuid -> Result Http.Error () -> Msg
createPgFileResultToMsg newNode id result =
    case result of
        Ok _ ->
            Touch newNode id

        Err error ->
            PrintNotification <| AlertNotification "Could not create a new file, try reloading the page" (httpErrorToString error)

createPgFolderResultToMsg : NewNode -> Uuid -> Result Http.Error () -> Msg
createPgFolderResultToMsg newNode id result =
    case result of
        Ok _ ->
            Mkdir newNode id

        Err error ->
            PrintNotification <| AlertNotification "Could not create folder, try reloading the page!" (httpErrorToString error)


-- * view


view : WhichDefaultView -> Model a -> Element Msg
view whichDefaultView model =
    el (box [ Background.color white
            , centerX
            , spacing 20
            , padding 30
            ]
       ) <|
        case whichDefaultView of
            DefaultView ->
                defaultView

            CreateDefaultFolderView ->
                createDefaultFolderView model

            CreateDefaultFileView ->
                createDefaultFileView model


-- ** default view


defaultView : Element Msg
defaultView =
    let
        newFolderLink =
            link primaryButtonAttrs
                { label =
                      iconWithAttr { defaultIconAttribute
                                       | title = " Create a new folder"
                                       , icon = "create_new_folder"
                                   }
                , url = href (PgPage (LandingView CreateDefaultFolderView))
                }

        newPgLink =
            link primaryButtonAttrs
                { label =
                      iconWithAttr { defaultIconAttribute
                                       | title = " Create a new request"
                                       , icon = "note_add"
                                   }
                , url = href (PgPage (LandingView CreateDefaultFileView))
                }
    in
    el [ centerX ]
        <| row [ spacing 20, centerY ]
              [ newFolderLink
              , text "or"
              , newPgLink
              ]


-- ** create default folder view


createDefaultFolderView : Model a -> Element Msg
createDefaultFolderView model =
    let
        createButton =
            case String.isEmpty model.pgNewNode.name of
                True -> none
                False ->
                    Input.button primaryButtonAttrs
                        { onPress = Just <|
                              GenerateRandomUUIDForFolder model.pgNewNode
                        , label =
                            iconWithAttr { defaultIconAttribute
                                             | title = "Create folder"
                                             , icon = "create_new_folder"
                                         }
                        }

        nameInput =
            Input.text []
                  { onChange = ChangeName
                  , text = model.pgNewNode.name
                  , placeholder = Just <| Input.placeholder [] (text "myFolder")
                  , label = Input.labelLeft [ centerY ] <| text "Folder name: "
                  }

        title =
            el [ Font.size 25, Font.underline ] (text "Create new folder")
    in
    column [ spacing 20 ]
        [ row [ width fill, centerY ]
              [ el [ alignLeft ] title
              , el [ alignRight ] (closeBuilderView model.page)
              ]
        , nameInput
        , folderTreeView model SelectFolder
        , el [ centerX ] createButton
        ]


-- ** create default file view


createDefaultFileView : Model a -> Element Msg
createDefaultFileView model =
    let
        createButton =
            case String.isEmpty model.pgNewNode.name of
                True -> none
                False ->
                    Input.button primaryButtonAttrs
                        { onPress = Just <|
                              GenerateRandomUUIDForFile model.pgNewNode
                        , label =
                            iconWithAttr { defaultIconAttribute
                                             | title = "Create pg"
                                             , icon = "note_add"
                                         }
                        }

        nameInput =
            Input.text []
                  { onChange = ChangeName
                  , text = model.pgNewNode.name
                  , placeholder = Just <| Input.placeholder [] (text "myPg")
                  , label = Input.labelLeft [ centerY ] <| text "Pg name: "
                  }
        title =
            el [ Font.size 25, Font.underline ] (text "Create new pg")
    in
    column [ spacing 20 ]
        [ row [ width fill, centerY ]
              [ el [ alignLeft ] title
              , el [ alignRight ] (closeBuilderView model.page)
              ]
        , nameInput
        , folderTreeView model SelectFolder
        , el [ centerX ] createButton
        ]


-- ** folder tree view


folderTreeView : Model a -> (Maybe Uuid -> msg) -> Element msg
folderTreeView model selectFolderMsg =
    let
        (PgCollection _ pgNodes) =
            model.pgCollection

        treeView =
            column [ spacing 10 ]
                [ text "Select a folder:"
                , folderView Nothing model (NotEdited "/") selectFolderMsg (nodeView model selectFolderMsg pgNodes)
                ]
    in
    treeView


nodeView : Model a -> (Maybe Uuid -> msg) -> List PgNode -> List (Element msg)
nodeView model selectFolderMsg pgNodes =
    case pgNodes of
        [] ->
            []

        node :: tail ->
            case node of
                File _ -> nodeView model selectFolderMsg tail
                Folder { id, name, children } ->
                    let
                        folderChildrenView =
                            nodeView model selectFolderMsg children

                        tailView =
                            nodeView model selectFolderMsg tail

                        currentFolderView =
                            folderView (Just id) model name selectFolderMsg folderChildrenView
                    in
                    currentFolderView :: tailView


-- ** folder view


folderView : Maybe Uuid -> Model a -> Editable String -> (Maybe Uuid -> msg) -> List (Element msg) -> Element msg
folderView mId model eName selectFolderMsg folderChildrenView =
    let
        label : String -> Element msg
        label title =
            iconWithAttr { defaultIconAttribute
                             | title = title
                             , iconSize = Nothing
                             , icon = "folder"
                             , iconVerticalAlign = Just "bottom"
                             , primIconColor =
                               case selected of
                                   True -> Just primaryColor
                                   False -> Nothing
                         }

        selected =
            model.pgNewNode.parentFolderId == mId

        selectedAttributes =
            case selected of
                False -> []
                True -> [ Font.bold ]

        selectFolderBtn : Element msg
        selectFolderBtn =
            Input.button selectedAttributes
                { onPress = Just (selectFolderMsg mId)
                , label = label (editedOrNotEditedValue eName)
                }
    in
    column [ spacing 10 ]
        [ selectFolderBtn
        , column [ paddingXY 20 0 ] folderChildrenView
        ]
