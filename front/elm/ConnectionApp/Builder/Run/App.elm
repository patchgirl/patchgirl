module ConnectionApp.Builder.Run.App exposing (..)

import Animation
import Api.Converter as Client
import Random
import Dict exposing (Dict)
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
import BuilderUtil exposing (..)
import PGBuilderApp.PGTree.App as PgTree
import BuilderUtil exposing(..)
import Browser.Navigation as Navigation
import Interpolator exposing(..)
import Application.Model as Application
import BuilderUtil exposing (..)
import List.Extra as List
import StringTemplate exposing(..)
import HttpError exposing(..)
import Interpolator exposing(..)
import Runner


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
    = PrintNotification Notification
    | AskSave
    | Save (List KeyValue)


-- * update


update : Msg -> Model a -> PgConnection -> (Model a, PgConnection, Cmd Msg)
update msg model connection =
    case msg of

        AskSave ->
            let
                updateKeyValues =
                    Debug.todo ""
                    -- List.map Client.convertConnectionKeyValueFromFrontToBack connection.keyValues

                newMsg =
                    Cmd.none
                    -- Client.putApiConnectionByConnectionIdKeyValue "" (getCsrfToken model.session) (Id connection.id) updateKeyValues (saveKeyValuesResultToMsg connection.keyValues)
            in
            ( model, connection, newMsg )

        Save keyValues ->
            let
                markKeyValueAsClean : KeyValue -> KeyValue
                markKeyValueAsClean keyValue =
                    { keyValue
                        | key = cleanEditable keyValue.key
                        , value = cleanEditable keyValue.value
                        , hidden = cleanEditable keyValue.hidden
                    }

                newConnection =
                    Debug.todo ""
                    --{ connection | keyValues = List.map markKeyValueAsClean keyValues }
            in
            ( model, newConnection, Cmd.none )

        PrintNotification notification ->
            let
                newModel =
                    { model | notification = Just notification }
            in
            ( newModel, connection, Cmd.none )


-- * util


mkNewDefaultKeyValue : Uuid -> KeyValue
mkNewDefaultKeyValue id =
    { id = id
    , key = NotEdited ""
    , value = NotEdited [ Sentence "" ]
    , hidden = NotEdited False
    }

saveKeyValuesResultToMsg : List KeyValue -> Result Http.Error () -> Msg
saveKeyValuesResultToMsg keyValues result =
    case result of
        Ok () ->
            Save keyValues

        Err err ->
            PrintNotification <| AlertNotification "Could not update key value, try reloading the page!" (httpErrorToString err)

removeIf : (a -> Bool) -> List a -> List a
removeIf predicate list =
    let
        fold : a -> List a -> List a
        fold e acc =
            case predicate e of
                True ->
                    acc

                False ->
                    acc ++ [ e ]
    in
    List.foldl fold [] list


-- * view


view : Model a -> PgConnection -> Element Msg
view model connection =
    let
        addNewKeyValueView =
            Input.button []
                { onPress = Nothing -- Just <| GenerateRandomUUIDForKeyValue
                , label =
                    el primaryButtonAttrs <|
                        iconWithAttr { defaultIconAttribute
                                         | title = " Add key value"
                                         , icon = "add_circle_outline"
                                     }
                }
    in
    column ( box  [ alignTop, centerX, alignTop, padding 30, spacing 30 ] ) <|
        [ titleView model connection
        , column [ spacing 5, centerX ] <|
            []
            --List.map viewKeyValue connection.keyValues
        , el [ centerX ] addNewKeyValueView
        ]


-- ** title view


titleView : Model a -> PgConnection -> Element Msg
titleView model connection =
    let
        isConnectionDirty =
            True
            --List.any isKeyValueDirty connection.keyValues

        mainActionButtonsView : Element Msg
        mainActionButtonsView =
            let
                inputParam =
                    [ Border.solid
                    , Border.color secondaryColor
                    , Border.width 1
                    , Border.rounded 5
                    , alignBottom
                    , Background.color secondaryColor
                    , paddingXY 10 10
                    ]
            in
            Input.button inputParam
                { onPress = Just <| AskSave
                , label = iconWithTextAndColor "save" "Save" primaryColor
                }
    in
    row [ paddingXY 0 10, spacing 10, width fill ]
        [ el [ paddingXY 11 11 ] <| iconWithTextAndColor "label" (editedOrNotEditedValue connection.name) secondaryColor
        , case isConnectionDirty of
            True ->
                el [ alignRight ] mainActionButtonsView

            False ->
                none
        , el [ alignRight ] <| closeBuilderView model.page
        ]


-- ** entry view


viewKeyValue : KeyValue -> Element Msg
viewKeyValue keyValue =
    let
        { id, key, value, hidden } =
            keyValue
    in
    wrappedRow [ spacing 20, centerX, width fill ]
        [         hideView keyValue
        ]





-- *** hide view


hideView : KeyValue -> Element Msg
hideView keyValue =
    let
        label =
            case (editedOrNotEditedValue keyValue.hidden) of
                True ->
                    iconWithAttr { defaultIconAttribute
                                     | icon = "toggle_on"
                                     , iconSize = Just "30px"
                                     , title = " hide value"
                                     , primIconColor = Nothing
                                     , iconVerticalAlign = Just "middle"
                                 }

                False ->
                    iconWithAttr { defaultIconAttribute
                                     | icon = "toggle_off"
                                     , iconSize = Just "30px"
                                     , title = " hide value"
                                     , primIconColor = Nothing
                                     , iconVerticalAlign = Just "middle"
                                 }
    in
    Input.button []
        { onPress = Nothing -- Just <| ToggleValueVisibility keyValue.id
        , label = label
        }
