module BuilderApp.BuilderTree.App exposing (..)

import BuilderApp.Model exposing (..)
import BuilderApp.BuilderTree.Message exposing (..)
import BuilderApp.BuilderTree.Util exposing (..)

import BuilderApp.Builder.App as Builder

import Http as Http
import Util.Maybe as Maybe
import Api.Generated as Client


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
  case msg of
    SetDisplayedBuilder idx ->
        let
            newModel = { model | selectedBuilderIndex = Just idx }
        in
            (newModel, Cmd.none)

    ToggleMenu idx ->
        let
            newDisplayedRequestNodeMenuIndex =
                case Maybe.exists model.displayedRequestNodeMenuIndex ((==) idx) of
                    True -> Nothing
                    False -> Just idx
            newModel = { model | displayedRequestNodeMenuIndex = newDisplayedRequestNodeMenuIndex }
        in
            (newModel, Cmd.none)

    ToggleFolder idx ->
        let
            (RequestCollection id requestNodes) = model.requestCollection
            newModel =
                { model
                    | requestCollection =
                      RequestCollection id (modifyRequestNode toggleFolder requestNodes idx)
                }
        in
            (newModel, Cmd.none)

    Mkdir idx ->
        let
            (RequestCollection id requestNodes) = model.requestCollection
            newModel =
                { model
                    | requestCollection =
                      RequestCollection id (modifyRequestNode mkdir requestNodes idx)
                }
        in
            (newModel, Cmd.none)

    Touch idx ->
        let
            (RequestCollection id requestNodes) = model.requestCollection
            newModel =
                { model
                    | requestCollection =
                      RequestCollection id (modifyRequestNode touch requestNodes idx)
                }
        in
            (newModel, Cmd.none)

    ShowRenameInput idx ->
        let
            (RequestCollection id requestNodes) = model.requestCollection
            newModel =
                { model
                    | requestCollection =
                      RequestCollection id (modifyRequestNode displayRenameInput requestNodes idx)
                }
        in
            (newModel, Cmd.none)

    ChangeName idx newName ->
        let
            (RequestCollection id requestNodes) = model.requestCollection
            newModel =
                { model
                    | requestCollection =
                      RequestCollection id (modifyRequestNode (tempRename newName) requestNodes idx)
                }
        in
            (newModel, Cmd.none)

    AskRename requestNodeIdx newName ->
        let
            (RequestCollection requestCollectionId requestNodes) =
                model.requestCollection

            payload =
                Client.UpdateRequestFolder { updateRequestNodeName = newName }

            newMsg =
                Client.putApiRequestCollectionByRequestCollectionIdRequestNodeByRequestNodeId "" "" requestCollectionId requestNodeIdx payload (newEnvironmentResultToMsg requestNodeIdx newName)
        in
            (model, newMsg)

    Rename idx newName ->
        let
            (RequestCollection id requestNodes) = model.requestCollection
            newModel =
                { model
                    | requestCollection =
                      RequestCollection id (modifyRequestNode (rename newName) requestNodes idx)
                }
        in
            (newModel, Cmd.none)

    Delete idx ->
        let
            (RequestCollection id requestNodes) = model.requestCollection
            newModel =
                { model
                    | requestCollection =
                        RequestCollection id (deleteRequestNode requestNodes idx)
                }
        in
            (newModel, Cmd.none)

    BuilderTreeServerError ->
        Debug.todo "error with builder tree"

    DoNothing ->
        (model, Cmd.none)


-- * util


newEnvironmentResultToMsg : Int -> String -> Result Http.Error () -> Msg
newEnvironmentResultToMsg idx newName result =
    case Debug.log "result" result of
        Ok _ ->
            Rename idx newName

        Err error ->
            BuilderTreeServerError
