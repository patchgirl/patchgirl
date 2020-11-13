module ConnectionApp.Builder.Edit.App exposing (..)

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
import BuilderUtil as Tree
import PGBuilderApp.PGTree.App as Tree
import BuilderUtil exposing(..)
import Browser.Navigation as Navigation
import List.Extra as List


-- * model


type alias Model a =
    { a
        | pgConnections : List PgConnection
        , selectedPgConnectionId : Maybe Int
        , selectedPgConnectionToRun : Maybe Int
        , displayedPgConnectionMenuId : Maybe Int
        , displayedConnectionBuilderView : BuilderView Int
        , newConnectionName : String
        , notification : Maybe Notification
        , page : Page
        , navigationKey : Navigation.Key
    }


-- * msg


type Msg
    -- rename
    = UpdateName Int String -- while focus is on the input
    | AskRename Int String
    | Rename Int String
    -- delete
    | AskDelete Int
    | Delete Int
    -- other
    | PrintNotification Notification


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
        UpdateName id newName ->
            let
                updateConnection old =
                    { old | name = changeEditedValue newName old.name }

                newConnections =
                    List.updateIf (\elem -> elem.id == id) updateConnection model.pgConnections

                newModel =
                    { model
                        | pgConnections = newConnections
                    }
            in
            ( newModel, Cmd.none )

        AskRename id newName ->
            let
                payload =
                    Debug.todo ""
                    --Client.UpdateConnection { updateConnectionName = newName }

                newMsg =
                    Cmd.none
                    --Client.putApiConnectionByConnectionId "" (getCsrfToken model.session) (Id id) payload (updateConnectionResultToMsg id newName)
            in
            ( model, newMsg )

        Rename id newName ->
            let
                updateConnection old =
                    { old
                        | name = NotEdited newName
                    }

                newConnections =
                    List.updateIf (\elem -> elem.id == id) updateConnection model.pgConnections

                newModel =
                    { model
                        | pgConnections = newConnections
                    }
            in
            ( newModel, Cmd.none )

        AskDelete id ->
            let
                newMsg =
                    Cmd.none
                        --Client.deleteApiConnectionByConnectionId "" (getCsrfToken model.session) (Id id) (deleteConnectionResultToMsg id)
            in
            ( model, newMsg )


        Delete id ->
            let
                newConnections =
                    List.filter (\elem -> elem.id /= id) model.pgConnections

                newModel =
                    { model
                        | pgConnections = newConnections
                    }

                newMsg =
                    Navigation.pushUrl model.navigationKey (href (ConnectionPage (LandingView DefaultView)))

            in
            ( newModel, newMsg )

        PrintNotification notification ->
            ( { model | notification = Just notification }, Cmd.none )


-- * util


newConnectionResultToMsg : Int -> String -> Result Http.Error () -> Msg
newConnectionResultToMsg id name result =
    case result of
        Ok () ->
            Rename id name

        Err err ->
            PrintNotification <| AlertNotification "Could not create a new connection, try reloading the page!" (httpErrorToString err)

updateConnectionResultToMsg : Int -> String -> Result Http.Error () -> Msg
updateConnectionResultToMsg id name result =
    case result of
        Ok () ->
            Rename id name

        Err err ->
            PrintNotification <| AlertNotification "Could not update connection, try reloading the page!" (httpErrorToString err)

deleteConnectionResultToMsg : Int -> Result Http.Error () -> Msg
deleteConnectionResultToMsg id result =
    case result of
        Ok () ->
            Delete id

        Err err ->
            PrintNotification <| AlertNotification "Could not delete connection, try reloading the page!" (httpErrorToString err)

-- * view


view : WhichEditView PgConnection -> Model a -> Element Msg
view whichEditView model =
    el ( box [ centerX, spacing 20, padding 30 ] ) <|
        case whichEditView of
            DefaultEditView connection ->
                defaultEditView model connection

            DeleteView connection ->
                deleteView model connection

            DuplicateView connection ->
                none


-- ** default view


defaultEditView : Model a -> PgConnection -> Element Msg
defaultEditView model connection =
    let
        renameBtn =
            Input.button primaryButtonAttrs
                { onPress = Just <|
                      AskRename connection.id (editedOrNotEditedValue connection.name)
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
                , url = href (ConnectionPage (EditView (DeleteView connection.id)))
                }

        renameInput =
            Input.text []
                  { onChange = UpdateName connection.id
                  , text = editedOrNotEditedValue connection.name
                  , placeholder = Just <| Input.placeholder [] (text "my connection")
                  , label = Input.labelLeft [ centerY ] <| text "connection: "
                  }

        title =
            el [ Font.size 25, Font.underline ] (text ("Edit connection: " ++ (editedOrNotEditedValue connection.name)))
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


deleteView : Model a -> PgConnection -> Element Msg
deleteView model connection =
    let
        areYouSure =
            text "Are you sure you want to delete this?"

        yesBtn =
            Input.button primaryButtonAttrs
                { onPress = Just <| AskDelete connection.id
                , label = text "Yes"
                }

        noBtn =
            link primaryButtonAttrs
                { url = href (ConnectionPage (EditView (DefaultEditView connection.id)))
                , label = text "No"
                }

        title =
            el [ Font.size 25, Font.underline ] <| text ("Delete " ++ (notEditedValue connection.name))
    in
    column [ spacing 20 ]
        [ row [ width fill, centerY ]
              [ el [ alignLeft ] title
              , el [ alignRight ] (closeBuilderView model.page)
              ]
        , areYouSure
        , row [ centerX, spacing 20 ] [ noBtn, yesBtn ]
        ]
