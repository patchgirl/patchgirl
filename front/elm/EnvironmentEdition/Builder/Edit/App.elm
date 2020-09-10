module EnvironmentEdition.Builder.Edit.App exposing (..)

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
import BuilderUtil as Tree
import PGBuilderApp.PGTree.App as Tree
import BuilderUtil exposing(..)
import Browser.Navigation as Navigation
import List.Extra as List


-- * model


type alias Model a =
    { a
        | environments : List Environment
        , newEnvironmentName : String
        , session : Session
        , notification : Maybe Notification
        , displayedEnvId : Maybe Uuid
        , displayedEnvironmentNodeMenuId : Maybe Uuid
        , displayedEnvironmentBuilderView : BuilderView Uuid
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
                updateEnv old =
                    { old | name = changeEditedValue newName old.name }

                mNewEnvs =
                    List.updateIf (\elem -> elem.id == id) updateEnv model.environments

                newModel =
                    { model
                        | environments = mNewEnvs
                    }
            in
            ( newModel, Cmd.none )

        AskRename id newName ->
            let
                payload =
                    Client.UpdateEnvironment { updateEnvironmentName = newName }

                newMsg =
                    Client.putApiEnvironmentByEnvironmentId "" (getCsrfToken model.session) id payload (updateEnvironmentResultToMsg id newName)
            in
            ( model, newMsg )

        Rename id newName ->
            let
                updateEnv old =
                    { old
                        | name = NotEdited newName
                        , showRenameInput = False
                    }

                newEnvs =
                    List.updateIf (\elem -> elem.id == id) updateEnv model.environments

                newModel =
                    { model
                        | environments = newEnvs
                    }
            in
            ( newModel, Cmd.none )

        AskDelete id ->
            let
                newMsg =
                    Client.deleteApiEnvironmentByEnvironmentId "" (getCsrfToken model.session) id (deleteEnvironmentResultToMsg id)
            in
            ( model, newMsg )


        Delete id ->
            let
                newEnvironments =
                    List.filter (\elem -> elem.id /= id) model.environments

                newModel =
                    { model
                        | environments = newEnvironments
                    }

                newMsg =
                    Navigation.pushUrl model.navigationKey (href (EnvPage2 (LandingView DefaultView)))

            in
            ( newModel, newMsg )

        PrintNotification notification ->
            ( { model | notification = Just notification }, Cmd.none )


-- * util


newEnvironmentResultToMsg : Uuid -> String -> Result Http.Error () -> Msg
newEnvironmentResultToMsg id name result =
    case result of
        Ok () ->
            Rename id name

        Err err ->
            PrintNotification <| AlertNotification "Could not create a new environment, try reloading the page!" (httpErrorToString err)

updateEnvironmentResultToMsg : Uuid -> String -> Result Http.Error () -> Msg
updateEnvironmentResultToMsg id name result =
    case result of
        Ok () ->
            Rename id name

        Err err ->
            PrintNotification <| AlertNotification "Could not update environment, try reloading the page!" (httpErrorToString err)

deleteEnvironmentResultToMsg : Uuid -> Result Http.Error () -> Msg
deleteEnvironmentResultToMsg id result =
    case result of
        Ok () ->
            Delete id

        Err err ->
            PrintNotification <| AlertNotification "Could not delete environment, try reloading the page!" (httpErrorToString err)

-- * view


view : WhichEditView Environment -> Model a -> Element Msg
view whichEditView model =
    el ( box [ centerX, spacing 20, padding 30 ] ) <|
        case whichEditView of
            DefaultEditView environment ->
                defaultEditView model environment

            DeleteView environment ->
                deleteView model environment


-- ** default view


defaultEditView : Model a -> Environment -> Element Msg
defaultEditView model environment =
    let
        renameBtn =
            Input.button primaryButtonAttrs
                { onPress = Just <|
                      AskRename environment.id (editedOrNotEditedValue environment.name)
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
                , url = href (EnvPage2 (EditView (DeleteView environment.id)))
                }

        renameInput =
            Input.text []
                  { onChange = UpdateName environment.id
                  , text = editedOrNotEditedValue environment.name
                  , placeholder = Just <| Input.placeholder [] (text "my environment")
                  , label = Input.labelLeft [ centerY ] <| text "environment: "
                  }

        title =
            el [ Font.size 25, Font.underline ] (text ("Edit environment: " ++ (editedOrNotEditedValue environment.name)))
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


deleteView : Model a -> Environment -> Element Msg
deleteView model environment =
    let
        areYouSure =
            text "Are you sure you want to delete this?"

        yesBtn =
            Input.button primaryButtonAttrs
                { onPress = Just <| AskDelete environment.id
                , label = text "Yes"
                }

        noBtn =
            link primaryButtonAttrs
                { url = href (EnvPage2 (EditView (DefaultEditView environment.id)))
                , label = text "No"
                }

        title =
            el [ Font.size 25, Font.underline ] <| text ("Delete " ++ (notEditedValue environment.name))
    in
    column [ spacing 20 ]
        [ row [ width fill, centerY ]
              [ el [ alignLeft ] title
              , el [ alignRight ] (closeBuilderView model.page)
              ]
        , areYouSure
        , row [ centerX, spacing 20 ] [ noBtn, yesBtn ]
        ]
