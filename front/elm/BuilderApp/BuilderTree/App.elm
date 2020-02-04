module BuilderApp.BuilderTree.App exposing (..)

import BuilderApp.Model exposing (..)
import BuilderApp.BuilderTree.Message exposing (..)
import BuilderApp.BuilderTree.Util exposing (..)

import BuilderApp.Builder.App as Builder

import Random
import Uuid
import Http as Http
import Util.Maybe as Maybe
import Api.Generated as Client


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
  case msg of


-- ** toggle builder/menu/folder


    SetDisplayedBuilder id ->
        let
            newModel = { model | selectedBuilderId = Just id }
        in
            (newModel, Cmd.none)

    ToggleMenu id ->
        let
            newDisplayedRequestNodeMenuIndex =
                case Maybe.exists model.displayedRequestNodeMenuId ((==) id) of
                    True -> Nothing -- menu already displayed
                    False -> Just id

            newModel =
                { model |
                      displayedRequestNodeMenuId = newDisplayedRequestNodeMenuIndex
                }
        in
            (newModel, Cmd.none)

    ToggleFolder id ->
        let
            (RequestCollection requestCollectionId requestNodes) =
                model.requestCollection

            newRequestNodes =
                List.map (modifyRequestNode2 id toggleFolder) requestNodes

            newModel =
                { model
                    | requestCollection =
                      RequestCollection requestCollectionId newRequestNodes
                }
        in
            (newModel, Cmd.none)


-- ** mkdir


    GenerateRandomUUIDForFolder parentNodeId ->
        let
            newMsg = Random.generate (AskMkdir parentNodeId) Uuid.uuidGenerator
        in
            (model, newMsg)

    AskMkdir parentNodeId newId ->
        let
            (RequestCollection requestCollectionId requestNodes) =
                model.requestCollection

            newRequestFolder =
                { newRequestFolderId = newId
                , newRequestFolderParentNodeId = parentNodeId
                , newRequestFolderName = "new folder"
                }

            newMsg =
                Client.postApiRequestCollectionByRequestCollectionIdRequestFolder "" "" requestCollectionId newRequestFolder (createRequestFolderResultToMsg parentNodeId newId)
        in
            (model, newMsg)

    Mkdir parentNodeId newId ->
        let
            (RequestCollection id requestNodes) =
                model.requestCollection

            newRequestNodes =
                List.map (modifyRequestNode2 parentNodeId (mkdir newId)) requestNodes

            newModel =
                { model
                    | requestCollection =
                      RequestCollection id newRequestNodes
                }
        in
            (newModel, Cmd.none)


-- ** touch


    GenerateRandomUUIDForFile parentNodeId ->
        let
            newMsg = Random.generate (AskTouch parentNodeId) Uuid.uuidGenerator
        in
            (model, newMsg)

    AskTouch parentNodeId newId ->
        let
            (RequestCollection requestCollectionId requestNodes) =
                model.requestCollection

            newRequestFile =
                   { newRequestFileId = newId
                   , newRequestFileParentNodeId = parentNodeId
                   }

            newMsg =
                Client.postApiRequestCollectionByRequestCollectionIdRequestFile "" "" requestCollectionId newRequestFile (newRequestFileResultToMsg parentNodeId newId)
        in
            (model, newMsg)

    Touch parentNodeId newId ->
        let
            (RequestCollection id requestNodes) = model.requestCollection

            newRequestNodes =
                List.map (modifyRequestNode2 parentNodeId (touch newId)) requestNodes

            newModel =
                { model
                    | requestCollection =
                      RequestCollection id newRequestNodes
                }
        in
            (newModel, Cmd.none)


-- ** rename


    ShowRenameInput id ->
        let
            (RequestCollection requestCollectionId requestNodes) =
                model.requestCollection

            newRequestNodes =
                List.map (modifyRequestNode2 id displayRenameInput) requestNodes

            newModel =
                { model
                    | requestCollection =
                      RequestCollection requestCollectionId newRequestNodes
                }
        in
            (newModel, Cmd.none)

    ChangeName id newName ->
        let
            (RequestCollection requestCollectionId requestNodes) = model.requestCollection

            newRequestNodes =
                List.map (modifyRequestNode2 id (tempRename newName)) requestNodes

            newModel =
                { model
                    | requestCollection =
                      RequestCollection requestCollectionId newRequestNodes
                }
        in
            (newModel, Cmd.none)

    AskRename id newName ->
        let
            (RequestCollection requestCollectionId requestNodes) =
                model.requestCollection

            payload =
                Client.UpdateRequestFolder { updateRequestNodeName = newName }

            newMsg =
                Client.putApiRequestCollectionByRequestCollectionIdRequestNodeByRequestNodeId "" "" requestCollectionId id payload (renameNodeResultToMsg id newName)
        in
            (model, newMsg)

    Rename id newName ->
        let
            (RequestCollection requestCollectionId requestNodes) =
                model.requestCollection

            newRequestNodes =
                List.map (modifyRequestNode2 id (rename newName)) requestNodes

            newModel =
                { model
                    | requestCollection =
                      RequestCollection requestCollectionId newRequestNodes
                }
        in
            (newModel, Cmd.none)


-- ** delete


    AskDelete id ->
        let
            (RequestCollection requestCollectionId _) = model.requestCollection

            newMsg =
                Client.deleteApiRequestCollectionByRequestCollectionIdRequestNodeByRequestNodeId "" "" requestCollectionId id (deleteRequestNodeResultToMsg id)
        in
            (model, newMsg)

    Delete id ->
        let
            (RequestCollection requestCollectionId requestNodes) = model.requestCollection

            newRequestNodes =
                List.concatMap (deleteRequestNode id) requestNodes

            newModel =
                { model
                    | requestCollection =
                        RequestCollection requestCollectionId newRequestNodes
                }
        in
            (newModel, Cmd.none)

-- ** root file


    GenerateRandomUUIDForRootFile ->
        let
            newMsg = Random.generate AskTouchRoot Uuid.uuidGenerator
        in
            (model, newMsg)

    AskTouchRoot newId ->
        let
            (RequestCollection requestCollectionId _) =
                model.requestCollection

            newRootRequestFile =
                Client.NewRootRequestFile { newRootRequestFileId = newId }

            newMsg =
                Client.postApiRequestCollectionByRequestCollectionIdRootRequestFile "" "" requestCollectionId newRootRequestFile (createRootRequestFileResultToMsg newId)
        in
            (model, newMsg)

    TouchRoot newId ->
        let
            (RequestCollection requestCollectionId requestNodes) =
                model.requestCollection

            newRequestNodes =
                mkDefaultFile newId :: requestNodes

            newModel =
                { model
                    | requestCollection =
                        RequestCollection requestCollectionId newRequestNodes
                }
        in
            (newModel, Cmd.none)


-- ** other


    BuilderTreeServerError ->
        Debug.todo "error with builder tree"


    DoNothing ->
        (model, Cmd.none)


-- * util


renameNodeResultToMsg : Uuid.Uuid -> String -> Result Http.Error () -> Msg
renameNodeResultToMsg id newName result =
    case result of
        Ok _ ->
            Rename id newName

        Err error ->
            BuilderTreeServerError

createRequestFolderResultToMsg : Uuid.Uuid -> Uuid.Uuid -> Result Http.Error () -> Msg
createRequestFolderResultToMsg parentNodeId id result =
    case result of
        Ok _ ->
            Mkdir parentNodeId id

        Err error ->
            BuilderTreeServerError

deleteRequestNodeResultToMsg : Uuid.Uuid -> Result Http.Error () -> Msg
deleteRequestNodeResultToMsg id result =
    case result of
        Ok _ ->
            Delete id

        Err error ->
            BuilderTreeServerError

newRequestFileResultToMsg : Uuid.Uuid -> Uuid.Uuid -> Result Http.Error () -> Msg
newRequestFileResultToMsg parentNodeId id result =
    case result of
        Ok _ ->
            Touch parentNodeId id

        Err error ->
            BuilderTreeServerError

createRootRequestFileResultToMsg : Uuid.Uuid -> Result Http.Error () -> Msg
createRootRequestFileResultToMsg id result =
    case result of
        Ok _ ->
            TouchRoot id

        Err error ->
            BuilderTreeServerError
