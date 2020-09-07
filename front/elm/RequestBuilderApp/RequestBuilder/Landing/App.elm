module RequestBuilderApp.RequestBuilder.Landing.App exposing (..)

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
import BuilderUtil exposing (..)
import Animation


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
                oldLandingRequestNewFolder =
                    model.requestNewNode

                newLandingRequestNewFolder =
                    { oldLandingRequestNewFolder | name = newName }

                newModel =
                    { model | requestNewNode = newLandingRequestNewFolder }
            in
            (newModel, Cmd.none)

        SelectFolder mId ->
            let
                oldLandingRequestNewNode =
                    model.requestNewNode

                newLandingRequestNewNode =
                    { oldLandingRequestNewNode | parentFolderId = mId }

                newModel =
                    { model | requestNewNode = newLandingRequestNewNode }
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
                (RequestCollection requestCollectionId requestNodes) =
                    model.requestCollection

                newMsg =
                    case newNode.parentFolderId of
                        Nothing ->
                            let
                                payload =
                                    { newRootRequestFolderId = newId
                                    , newRootRequestFolderName = newNode.name
                                    }
                            in
                            Client.postApiRequestCollectionByRequestCollectionIdRootRequestFolder "" "" requestCollectionId payload (createRequestFolderResultToMsg newNode newId)

                        Just folderId ->
                            let
                                payload =
                                    { newRequestFolderId = newId
                                    , newRequestFolderParentNodeId = folderId
                                    , newRequestFolderName = newNode.name
                                    }
                            in
                            Client.postApiRequestCollectionByRequestCollectionIdRequestFolder "" "" requestCollectionId payload (createRequestFolderResultToMsg newNode newId)

            in
            ( model, newMsg )

        Mkdir newNode newId ->
            let
                (RequestCollection id requestNodes) =
                    model.requestCollection

                newRequestNodes =
                    case newNode.parentFolderId of
                        Nothing ->
                            requestNodes ++ [ mkDefaultFolder newNode newId ]

                        Just folderId ->
                            List.map (RequestTree.modifyRequestNode folderId (mkdir newNode newId)) requestNodes

                newModel =
                    { model
                        | requestCollection =
                            RequestCollection id newRequestNodes
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
                (RequestCollection requestCollectionId requestNodes) =
                    model.requestCollection

                newMsg =
                    case newNode.parentFolderId of
                        Nothing ->
                            let
                                payload =
                                       { newRootRequestFileId = newId
                                       , newRootRequestFileName = newNode.name
                                       }

                            in
                            Client.postApiRequestCollectionByRequestCollectionIdRootRequestFile "" "" requestCollectionId payload (createRequestFileResultToMsg newNode newId)

                        Just folderId ->
                            let
                                payload =
                                    { newRequestFileId = newId
                                    , newRequestFileParentNodeId = folderId
                                    , newRequestFileName = newNode.name
                                    }
                            in
                            Client.postApiRequestCollectionByRequestCollectionIdRequestFile "" "" requestCollectionId payload (createRequestFileResultToMsg newNode newId)

            in
            ( model, newMsg )

        Touch newNode newId ->
            let
                (RequestCollection id requestNodes) =
                    model.requestCollection

                newRequestNodes =
                    case newNode.parentFolderId of
                        Nothing ->
                            requestNodes ++ [ mkDefaultFile newNode newId ]

                        Just folderId ->
                            List.map (RequestTree.modifyRequestNode folderId (touch newNode newId)) requestNodes

                newModel =
                    { model
                        | requestCollection =
                            RequestCollection id newRequestNodes
                    }
            in
            ( newModel, Cmd.none )

        PrintNotification notification ->
            ( { model | notification = Just notification }, Cmd.none )


-- * util


-- ** tree


touch : NewNode -> Uuid -> RequestNode -> RequestNode
touch newNode id parentNode =
    case parentNode of
        (File _) as file ->
            file

        Folder folder ->
            let
                (Children children) =
                    folder.children
            in
            Folder
                { folder
                    | children = Children (mkDefaultFile newNode id :: children)
                    , open = True
                }

mkdir : NewNode -> Uuid -> RequestNode -> RequestNode
mkdir newNode id node =
    case node of
        (File _) as file ->
            file

        Folder folder ->
            let
                (Children children) =
                    folder.children
            in
            Folder
                { folder
                    | children = Children (mkDefaultFolder newNode id :: children)
                    , open = True
                }

mkDefaultFolder : NewNode -> Uuid -> RequestNode
mkDefaultFolder newNode id =
    Folder
        { id = id
        , name = NotEdited newNode.name
        , open = False
        , children = Children []
        }


mkDefaultFile : NewNode -> Uuid -> RequestNode
mkDefaultFile newNode id =
    File
        { id = id
        , name = NotEdited newNode.name
        , httpUrl = NotEdited ""
        , httpMethod = NotEdited HttpGet
        , httpHeaders = NotEdited []
        , httpBody = NotEdited ""
        , showResponseView = False
        , whichResponseView = BodyResponseView
        , requestComputationResult = Nothing
        , runRequestIconAnimation = Animation.style []
        }

-- ** msg handling


createRequestFileResultToMsg : NewNode -> Uuid -> Result Http.Error () -> Msg
createRequestFileResultToMsg newNode id result =
    case result of
        Ok _ ->
            Touch newNode id

        Err error ->
            PrintNotification <| AlertNotification "Could not create a new file, try reloading the page" (httpErrorToString error)

createRequestFolderResultToMsg : NewNode -> Uuid -> Result Http.Error () -> Msg
createRequestFolderResultToMsg newNode id result =
    case result of
        Ok _ ->
            Mkdir newNode id

        Err error ->
            PrintNotification <| AlertNotification "Could not create folder, try reloading the page!" (httpErrorToString error)


-- * view


view : WhichDefaultView -> Model a -> Element Msg
view whichDefaultView model =
    el [ Background.color white
           , boxShadow
           , centerX
           , spacing 20
           , padding 30
           ] <|
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
                , url = href (ReqPage (LandingView CreateDefaultFolderView))
                }

        newRequestLink =
            link primaryButtonAttrs
                { label =
                      iconWithAttr { defaultIconAttribute
                                       | title = " Create a new request"
                                       , icon = "note_add"
                                   }
                , url = href (ReqPage (LandingView CreateDefaultFileView))
                }
    in
    el [ centerX ]
        <| row [ spacing 20, centerY ]
              [ newFolderLink
              , text "or"
              , newRequestLink
              ]


-- ** create default folder view


createDefaultFolderView : Model a -> Element Msg
createDefaultFolderView model =
    let
        createButton =
            case String.isEmpty model.requestNewNode.name of
                True -> none
                False ->
                    Input.button primaryButtonAttrs
                        { onPress = Just <|
                              GenerateRandomUUIDForFolder model.requestNewNode
                        , label =
                            iconWithAttr { defaultIconAttribute
                                             | title = "Create folder"
                                             , icon = "create_new_folder"
                                         }
                        }

        nameInput =
            Input.text []
                  { onChange = ChangeName
                  , text = model.requestNewNode.name
                  , placeholder = Just <| Input.placeholder [] (text "myFolder")
                  , label = Input.labelLeft [ centerY ] <| text "Folder name: "
                  }

        title =
            el [ Font.size 25, Font.underline ] (text "Create new folder")
    in
    column [ spacing 20 ]
        [ row [ width fill, centerY ]
              [ el [ alignLeft ] title
              , el [ alignRight ] closeBuilderView
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
            case String.isEmpty model.requestNewNode.name of
                True -> none
                False ->
                    Input.button primaryButtonAttrs
                        { onPress = Just <|
                              GenerateRandomUUIDForFile model.requestNewNode
                        , label =
                            iconWithAttr { defaultIconAttribute
                                             | title = "Create request"
                                             , icon = "note_add"
                                         }
                        }

        nameInput =
            Input.text []
                  { onChange = ChangeName
                  , text = model.requestNewNode.name
                  , placeholder = Just <| Input.placeholder [] (text "myRequest")
                  , label = Input.labelLeft [ centerY ] <| text "Request name: "
                  }
        title =
            el [ Font.size 25, Font.underline ] (text "Create new request")
    in
    column [ spacing 20 ]
        [ row [ width fill, centerY ]
              [ el [ alignLeft ] title
              , el [ alignRight ] closeBuilderView
              ]
        , nameInput
        , folderTreeView model SelectFolder
        , el [ centerX ] createButton
        ]


-- ** folder tree view


folderTreeView : Model a -> (Maybe Uuid -> msg) -> Element msg
folderTreeView model selectFolderMsg =
    let
        (RequestCollection _ requestNodes) =
            model.requestCollection

        treeView =
            column [ spacing 10 ]
                [ text "Select a folder:"
                , folderView Nothing model (NotEdited "/") selectFolderMsg (nodeView model selectFolderMsg requestNodes)
                ]
    in
    treeView


nodeView : Model a -> (Maybe Uuid -> msg) -> List RequestNode -> List (Element msg)
nodeView model selectFolderMsg requestNodes =
    case requestNodes of
        [] ->
            []

        node :: tail ->
            case node of
                File _ -> nodeView model selectFolderMsg tail
                Folder { id, name, children } ->
                    let
                        (Children c) =
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
            model.requestNewNode.parentFolderId == mId

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
