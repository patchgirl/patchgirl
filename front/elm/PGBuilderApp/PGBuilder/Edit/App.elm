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
    -- rename
    = UpdateName Uuid String -- while focus is on the input
    | AskRename Uuid String
    | Rename Uuid String
    -- delete
    | AskDelete Uuid
    | Delete Uuid
    -- other
    | PrintNotification Notification


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
        UpdateName id newName ->
            let
                (PgCollection pgCollectionId pgNodes) =
                    model.pgCollection

                newPgNodes =
                    List.map (Tree.modifyPgNode id (Tree.tempRename newName)) pgNodes

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
                    List.map (Tree.modifyPgNode id (Tree.rename newName)) pgNodes

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

            DuplicateView pgNode ->
                none


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
