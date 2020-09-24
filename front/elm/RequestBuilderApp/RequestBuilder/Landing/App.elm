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
import BuilderUtil exposing (..)
import Animation


-- * model


type alias Model a =
    { a
        | notification : Maybe Notification
        , requestCollection : RequestCollection
        , requestNewNode : NewNode
        , page: Page
    }


-- * msg


type Msg
    = SelectFolder Uuid
    | SelectRootFolder
    | ChangeName String
    -- mkdir
    | GenerateRandomUUIDForFolder NewNode
    | AskMkdir NewNode Uuid
    | Mkdir NewNode Uuid
    -- touch
    | GenerateRandomUUIDForFile NewNode
    | AskTouch NewNode Uuid
    | Touch RequestFileRecord (Maybe Uuid)
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

        SelectRootFolder ->
            let
                oldLandingRequestNewNode =
                    model.requestNewNode

                newLandingRequestNewNode =
                    { oldLandingRequestNewNode | parentFolderId = Nothing }

                newModel =
                    { model | requestNewNode = newLandingRequestNewNode }
            in
            (newModel, Cmd.none)

        SelectFolder id ->
            let
                oldLandingRequestNewNode =
                    model.requestNewNode

                newLandingRequestNewNode =
                    { oldLandingRequestNewNode | parentFolderId = Just id }

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
                (RequestCollection requestCollectionId _) =
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

                defaultFolder =
                    mkDefaultRequestFolder newId

                newFolder =
                    { defaultFolder | name = NotEdited newNode.name }

                newRequestNodes =
                    case newNode.parentFolderId of
                        Nothing ->
                            requestNodes ++ [ Folder newFolder ]

                        Just folderId ->
                            List.map (modifyRequestNode folderId (mkdirRequest newFolder)) requestNodes

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
                (RequestCollection requestCollectionId _) =
                    model.requestCollection

                newFile =
                    mkDefaultRequestFile newId

                newMsg =
                    case newNode.parentFolderId of
                        Nothing ->
                            let
                                payload =
                                       { newRootRequestFileId = newFile.id
                                       , newRootRequestFileName = notEditedValue newFile.name
                                       , newRootRequestFileHttpUrl = notEditedValue newFile.httpUrl
                                       , newRootRequestFileMethod = notEditedValue newFile.httpMethod |> Client.convertMethodFromFrontToBack
                                       , newRootRequestFileHeaders = notEditedValue newFile.httpHeaders
                                       , newRootRequestFileBody = notEditedValue newFile.httpBody
                                       }
                            in
                            Client.postApiRequestCollectionByRequestCollectionIdRootRequestFile "" "" requestCollectionId payload (createRequestFileResultToMsg newFile newNode.parentFolderId)

                        Just folderId ->
                            let
                                payload =
                                    { newRequestFileId = newFile.id
                                    , newRequestFileParentNodeId = folderId
                                    , newRequestFileName = notEditedValue newFile.name
                                    , newRequestFileHttpUrl = notEditedValue newFile.httpUrl
                                    , newRequestFileMethod = notEditedValue newFile.httpMethod |> Client.convertMethodFromFrontToBack
                                    , newRequestFileHeaders = notEditedValue newFile.httpHeaders
                                    , newRequestFileBody = notEditedValue newFile.httpBody
                                    }
                            in
                            Client.postApiRequestCollectionByRequestCollectionIdRequestFile "" "" requestCollectionId payload (createRequestFileResultToMsg newFile newNode.parentFolderId)

            in
            ( model, newMsg )

        Touch newFile mParentId ->
            let
                (RequestCollection id requestNodes) =
                    model.requestCollection

                newRequestNodes =
                    case mParentId of
                        Nothing ->
                            File newFile :: requestNodes

                        Just folderId ->
                            List.map (modifyRequestNode folderId (touchRequest (File newFile))) requestNodes

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



createRequestFileResultToMsg : RequestFileRecord -> Maybe Uuid -> Result Http.Error () -> Msg
createRequestFileResultToMsg newFile mParentId result =
    case result of
        Ok _ ->
            Touch newFile mParentId

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

        (RequestCollection _ nodes) =
            model.requestCollection
    in
    column [ spacing 20 ]
        [ row [ width fill, centerY ]
              [ el [ alignLeft ] title
              , el [ alignRight ] (closeBuilderView model.page)
              ]
        , nameInput
        , folderTreeView nodes model.requestNewNode.parentFolderId SelectRootFolder SelectFolder
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

        (RequestCollection _ nodes) =
            model.requestCollection
    in
    column [ spacing 20 ]
        [ row [ width fill, centerY ]
              [ el [ alignLeft ] title
              , el [ alignRight ] (closeBuilderView model.page)
              ]
        , nameInput
        , folderTreeView nodes model.requestNewNode.parentFolderId SelectRootFolder SelectFolder
        , el [ centerX ] createButton
        ]
