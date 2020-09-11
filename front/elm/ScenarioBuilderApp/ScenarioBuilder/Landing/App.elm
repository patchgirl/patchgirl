module ScenarioBuilderApp.ScenarioBuilder.Landing.App exposing (..)

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
import BuilderUtil exposing(..)
import BuilderUtil exposing (..)
import Animation


-- * model


type alias Model a =
    { a
        | notification : Maybe Notification
        , scenarioCollection : ScenarioCollection
        , scenarioNewNode : NewNode
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
                oldLandingScenarioNewFolder =
                    model.scenarioNewNode

                newLandingScenarioNewFolder =
                    { oldLandingScenarioNewFolder | name = newName }

                newModel =
                    { model | scenarioNewNode = newLandingScenarioNewFolder }
            in
            (newModel, Cmd.none)

        SelectFolder mId ->
            let
                oldLandingScenarioNewNode =
                    model.scenarioNewNode

                newLandingScenarioNewNode =
                    { oldLandingScenarioNewNode | parentFolderId = mId }

                newModel =
                    { model | scenarioNewNode = newLandingScenarioNewNode }
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
                (ScenarioCollection scenarioCollectionId scenarioNodes) =
                    model.scenarioCollection

                newMsg =
                    case newNode.parentFolderId of
                        Nothing ->
                            let
                                payload =
                                     { newRootScenarioFolderId = newId
                                     , newRootScenarioFolderName = newNode.name
                                     }
                            in
                            Client.postApiScenarioCollectionByScenarioCollectionIdRootScenarioFolder "" "" scenarioCollectionId payload (createScenarioFolderResultToMsg newNode newId)

                        Just folderId ->
                            let
                                payload =
                                    { newScenarioFolderId = newId
                                    , newScenarioFolderParentNodeId = folderId
                                    , newScenarioFolderName = newNode.name
                                    }
                            in
                            Client.postApiScenarioCollectionByScenarioCollectionIdScenarioFolder "" "" scenarioCollectionId payload (createScenarioFolderResultToMsg newNode newId)

            in
            ( model, newMsg )

        Mkdir newNode newId ->
            let
                (ScenarioCollection id scenarioNodes) =
                    model.scenarioCollection

                newScenarioNodes =
                    case newNode.parentFolderId of
                        Nothing ->
                            scenarioNodes ++ [ mkDefaultScenarioFolder newNode newId ]

                        Just folderId ->
                            List.map (modifyScenarioNode folderId (mkdirScenario newNode newId)) scenarioNodes

                newModel =
                    { model
                        | scenarioCollection =
                            ScenarioCollection id newScenarioNodes
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
                (ScenarioCollection scenarioCollectionId scenarioNodes) =
                    model.scenarioCollection

                newMsg =
                    case newNode.parentFolderId of
                        Nothing ->
                            let
                                payload =
                                    Debug.todo ""
                            in
                            Client.postApiScenarioCollectionByScenarioCollectionIdRootScenarioFile "" "" scenarioCollectionId payload (createScenarioFileResultToMsg newNode newId)

                        Just folderId ->
                            let
                                payload =
                                    Debug.todo ""
                            in
                            Client.postApiScenarioCollectionByScenarioCollectionIdScenarioFile "" "" scenarioCollectionId payload (createScenarioFileResultToMsg newNode newId)

            in
            ( model, newMsg )

        Touch newNode newId ->
            let
                (ScenarioCollection id scenarioNodes) =
                    model.scenarioCollection

                newScenarioNode =
                    mkDefaultScenarioFile newNode newId

                newScenarioNodes =
                    case newNode.parentFolderId of
                        Nothing ->
                            scenarioNodes ++ [ newScenarioNode ]

                        Just folderId ->
                            List.map (modifyScenarioNode folderId (touchScenario newScenarioNode)) scenarioNodes

                newModel =
                    { model
                        | scenarioCollection =
                            ScenarioCollection id newScenarioNodes
                    }
            in
            ( newModel, Cmd.none )

        PrintNotification notification ->
            ( { model | notification = Just notification }, Cmd.none )


-- * util


-- ** tree


mkdirScenario : NewNode -> Uuid -> ScenarioNode -> ScenarioNode
mkdirScenario newNode id node =
    case node of
        (File _) as file ->
            file

        Folder folder ->
            let
                (ScenarioChildren children) =
                    folder.children
            in
            Folder
                { folder
                    | children = ScenarioChildren (mkDefaultScenarioFolder newNode id :: children)
                    , open = True
                }

mkDefaultScenarioFolder : NewNode -> Uuid -> ScenarioNode
mkDefaultScenarioFolder newNode id =
    Folder
        { id = id
        , name = NotEdited newNode.name
        , open = False
        , children = ScenarioChildren []
        }


mkDefaultScenarioFile : NewNode -> Uuid -> ScenarioNode
mkDefaultScenarioFile newNode id =
    File
        { id = id
        , environmentId = NotEdited Nothing
        , name = NotEdited newNode.name
        , scenes = []
        }



-- ** msg handling


createScenarioFileResultToMsg : NewNode -> Uuid -> Result Http.Error () -> Msg
createScenarioFileResultToMsg newNode id result =
    case result of
        Ok _ ->
            Touch newNode id

        Err error ->
            PrintNotification <| AlertNotification "Could not create a new file, try reloading the page" (httpErrorToString error)

createScenarioFolderResultToMsg : NewNode -> Uuid -> Result Http.Error () -> Msg
createScenarioFolderResultToMsg newNode id result =
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
                , url = href (ScenarioPage (RichLandingView CreateDefaultFolderView))
                }

        newScenarioLink =
            link primaryButtonAttrs
                { label =
                      iconWithAttr { defaultIconAttribute
                                       | title = " Create a new scenario"
                                       , icon = "note_add"
                                   }
                , url = href (ScenarioPage (RichLandingView CreateDefaultFileView))
                }
    in
    el [ centerX ]
        <| row [ spacing 20, centerY ]
              [ newFolderLink
              , text "or"
              , newScenarioLink
              ]


-- ** create default folder view


createDefaultFolderView : Model a -> Element Msg
createDefaultFolderView model =
    let
        createButton =
            case String.isEmpty model.scenarioNewNode.name of
                True -> none
                False ->
                    Input.button primaryButtonAttrs
                        { onPress = Just <|
                              GenerateRandomUUIDForFolder model.scenarioNewNode
                        , label =
                            iconWithAttr { defaultIconAttribute
                                             | title = "Create folder"
                                             , icon = "create_new_folder"
                                         }
                        }

        nameInput =
            Input.text []
                  { onChange = ChangeName
                  , text = model.scenarioNewNode.name
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
            case String.isEmpty model.scenarioNewNode.name of
                True -> none
                False ->
                    Input.button primaryButtonAttrs
                        { onPress = Just <|
                              GenerateRandomUUIDForFile model.scenarioNewNode
                        , label =
                            iconWithAttr { defaultIconAttribute
                                             | title = "Create scenario"
                                             , icon = "note_add"
                                         }
                        }

        nameInput =
            Input.text []
                  { onChange = ChangeName
                  , text = model.scenarioNewNode.name
                  , placeholder = Just <| Input.placeholder [] (text "myScenario")
                  , label = Input.labelLeft [ centerY ] <| text "Scenario name: "
                  }
        title =
            el [ Font.size 25, Font.underline ] (text "Create new scenario")
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
        (ScenarioCollection _ scenarioNodes) =
            model.scenarioCollection

        treeView =
            column [ spacing 10 ]
                [ text "Select a folder:"
                , folderView Nothing model (NotEdited "/") selectFolderMsg (nodeView model selectFolderMsg scenarioNodes)
                ]
    in
    treeView


nodeView : Model a -> (Maybe Uuid -> msg) -> List ScenarioNode -> List (Element msg)
nodeView model selectFolderMsg scenarioNodes =
    case scenarioNodes of
        [] ->
            []

        node :: tail ->
            case node of
                File _ -> nodeView model selectFolderMsg tail
                Folder { id, name, children } ->
                    let
                        (ScenarioChildren c) =
                            children

                        folderChildrenView =
                            nodeView model selectFolderMsg c

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
            model.scenarioNewNode.parentFolderId == mId

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
