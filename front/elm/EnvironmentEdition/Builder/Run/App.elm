module EnvironmentEdition.Builder.Run.App exposing (..)

import Animation
import Api.Converter as Client
import Random
import Dict exposing (Dict)
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
    = GenerateRandomUUIDForKeyValue
    | AskCreateKeyValue Uuid
    | CreateKeyValue (List KeyValue)
    | PrintNotification Notification
    | PromptKey Uuid String
    | PromptValue Uuid String
    | AskSave
    | Save (List KeyValue)
    | AskDeleteKeyValue Uuid
    | DeleteKeyValue Uuid
    | ToggleValueVisibility Uuid


-- * update


update : Msg -> Model a -> Environment -> (Model a, Environment, Cmd Msg)
update msg model environment =
    case msg of

        GenerateRandomUUIDForKeyValue ->
            let
                newMsg =
                    Random.generate AskCreateKeyValue Uuid.uuidGenerator
            in
            ( model, environment, newMsg )

        AskCreateKeyValue newId ->
            let
                newKeyValue =
                    mkNewDefaultKeyValue newId

                newKeyValues =
                    environment.keyValues ++ [ newKeyValue ]

                updateKeyValues =
                    List.map Client.convertEnvironmentKeyValueFromFrontToBack newKeyValues

                newMsg =
                    Client.putApiEnvironmentByEnvironmentIdKeyValue "" (getCsrfToken model.session) environment.id updateKeyValues (updateKeyValuesResultToMsg newKeyValues)
            in
            ( model, environment, newMsg )

        CreateKeyValue newKeyValues ->
            let
                newEnvironment =
                    { environment | keyValues = newKeyValues }
            in
            ( model, newEnvironment, Cmd.none )

        PromptKey id newKey ->
            let
                newKeyValues =
                    List.updateIf (\e -> e.id == id) (\e -> { e | key = changeEditedValue newKey e.key }) environment.keyValues

                newEnvironment =
                    { environment | keyValues = newKeyValues }
            in
            ( model, newEnvironment, Cmd.none )

        PromptValue id newValue ->
            let
                newKeyValues =
                    List.updateIf (\e -> e.id == id) (\e -> { e | value = changeEditedValue (stringToTemplate newValue) e.value }) environment.keyValues

                newEnvironment =
                    { environment | keyValues = newKeyValues }
            in
            ( model, newEnvironment, Cmd.none )

        AskDeleteKeyValue id ->
            let
                newMsg =
                    Client.deleteApiEnvironmentByEnvironmentIdKeyValueByKeyValueId "" (getCsrfToken model.session) environment.id id (deleteKeyValueResultToMsg id)
            in
            ( model, environment, newMsg )

        DeleteKeyValue id ->
            let
                newKeyValues =
                    removeIf (\e -> e.id == id) environment.keyValues

                newEnvironment =
                    { environment | keyValues = newKeyValues }
            in
            ( model, newEnvironment, Cmd.none )

        ToggleValueVisibility id ->
            let
                toggleVisibility : Editable Bool -> Editable Bool
                toggleVisibility editable =
                    changeEditedValue (editedOrNotEditedValue editable |> not) editable

                newKeyValues =
                    List.updateIf (\e -> e.id == id) (\e -> { e | hidden = toggleVisibility e.hidden }) environment.keyValues

                newEnvironment =
                    { environment | keyValues = newKeyValues }
            in
            ( model, newEnvironment, Cmd.none )

        AskSave ->
            let
                updateKeyValues =
                    List.map Client.convertEnvironmentKeyValueFromFrontToBack environment.keyValues

                newMsg =
                    Client.putApiEnvironmentByEnvironmentIdKeyValue "" (getCsrfToken model.session) environment.id updateKeyValues (saveKeyValuesResultToMsg environment.keyValues)
            in
            ( model, environment, newMsg )

        Save keyValues ->
            let
                markKeyValueAsClean : KeyValue -> KeyValue
                markKeyValueAsClean keyValue =
                    { keyValue
                        | key = cleanEditable keyValue.key
                        , value = cleanEditable keyValue.value
                        , hidden = cleanEditable keyValue.hidden
                    }

                newEnvironment =
                    { environment | keyValues = List.map markKeyValueAsClean keyValues }
            in
            ( model, newEnvironment, Cmd.none )

        PrintNotification notification ->
            let
                newModel =
                    { model | notification = Just notification }
            in
            ( newModel, environment, Cmd.none )


-- * util


mkNewDefaultKeyValue : Uuid -> KeyValue
mkNewDefaultKeyValue id =
    { id = id
    , key = NotEdited ""
    , value = NotEdited [ Sentence "" ]
    , hidden = NotEdited False
    }

updateKeyValuesResultToMsg : List KeyValue -> Result Http.Error () -> Msg
updateKeyValuesResultToMsg keyValues result =
    case result of
        Ok () ->
            CreateKeyValue keyValues

        Err err ->
            PrintNotification <| AlertNotification "Could not update key value, try reloading the page!" (httpErrorToString err)

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


deleteKeyValueResultToMsg : Uuid -> Result Http.Error () -> Msg
deleteKeyValueResultToMsg id result =
    case result of
        Ok () ->
            DeleteKeyValue id

        Err err ->
            PrintNotification <| AlertNotification "Could not delete key value, try reloading the page" (httpErrorToString err)


-- * view


view : Model a -> Environment -> Element Msg
view model environment =
    let
        addNewKeyValueView =
            Input.button []
                { onPress = Just <| GenerateRandomUUIDForKeyValue
                , label =
                    el primaryButtonAttrs <|
                        iconWithAttr { defaultIconAttribute
                                         | title = " Add key value"
                                         , icon = "add_circle_outline"
                                     }
                }
    in
    column ( box  [ alignTop, centerX, alignTop, padding 30, spacing 30 ] ) <|
        [ titleView model environment
        , column [ spacing 5, centerX ] <|
            List.map viewKeyValue environment.keyValues
        , el [ centerX ] addNewKeyValueView
        ]


-- ** title view


titleView : Model a -> Environment -> Element Msg
titleView model environment =
    let
        isEnvironmentDirty =
            List.any isKeyValueDirty environment.keyValues

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
        [ el [ paddingXY 11 11 ] <| iconWithTextAndColor "label" (editedOrNotEditedValue environment.name) secondaryColor
        , case isEnvironmentDirty of
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

        deleteView : Element Msg
        deleteView =
            Input.button []
                { onPress = Just <| AskDeleteKeyValue keyValue.id
                , label = el [] deleteIcon
                }
    in
    wrappedRow [ spacing 20, centerX, width fill ]
        [ Input.text [ centerY ]
              { onChange = PromptKey id
              , text = editedOrNotEditedValue key
              , placeholder = Just <| Input.placeholder [ centerY ] (text "key")
              , label = Input.labelLeft [ centerY ] (text "Key: ")
              }
        , case editedOrNotEditedValue hidden of
              False ->
                  Input.text [ centerY, width (px 250) ]
                      { onChange = PromptValue id
                      , text = templatedStringAsString (editedOrNotEditedValue value)
                      , placeholder = Just <| Input.placeholder [ centerY ] (text "key")
                      , label = Input.labelLeft [ centerY ] (text "Value: ")
                      }

              True ->
                  Input.newPassword [ centerY, width (px 250) ]
                      { onChange = PromptValue id
                      , text = templatedStringAsString (editedOrNotEditedValue value)
                      , placeholder = Just <| Input.placeholder [ centerY ] (text "value")
                      , label = Input.labelLeft [ centerY ] (text "Value: ")
                      , show = False
                      }
        , deleteView
        , hideView keyValue
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
        { onPress = Just <| ToggleValueVisibility keyValue.id
        , label = label
        }
