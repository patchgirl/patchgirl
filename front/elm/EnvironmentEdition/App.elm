module EnvironmentEdition.App exposing (..)

import Api.Converter as Client
import Api.WebGeneratedClient as Client
import Application.Type exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Page exposing (..)
import Element.Input as Input
import Http
import List.Extra as List
import Util exposing (..)
import StringTemplate exposing(..)
import Browser.Navigation as Navigation
import HttpError exposing (..)
import Uuid exposing (Uuid)


-- *  environment edition


-- ** model


type alias Model a =
    { a
        | environments : List Environment
        , session : Session
        , notification : Maybe Notification
        , displayedEnvId : Maybe Uuid
        , navigationKey : Navigation.Key
    }


defaultEnvironment : Environment
defaultEnvironment =
    { id = Debug.todo ""
    , name = NotEdited "new environment"
    , showRenameInput = False
    , keyValues = []
    }



-- ** message


type Msg
    = EnvironmentKeyValueEditionMsg KeyValueMsg
    | AskEnvironmentCreation String
    | EnvironmentCreated Uuid String
    | ChangeName Uuid String
    | AskRename Uuid String
    | EnvironmentRenamed Uuid String
    | AskDelete Uuid
    | EnvironmentDeleted Uuid
    | PrintNotification Notification
    | ShowRenameInput Uuid



-- ** update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        AskEnvironmentCreation name ->
            let
                payload =
                    { newEnvironmentId = Debug.todo ""
                    , newEnvironmentName = name
                    }

                newMsg =
                    Client.postApiEnvironment "" (getCsrfToken model.session) payload (newEnvironmentResultToMsg name)
            in
            ( model, newMsg )

        EnvironmentCreated id name ->
            let
                newEnvironment =
                    { id = id
                    , name = NotEdited name
                    , showRenameInput = True
                    , keyValues = []
                    }

                newEnvironments =
                    model.environments ++ [ newEnvironment ]

                newModel =
                    { model | environments = newEnvironments }
            in
            ( newModel, Cmd.none )

        PrintNotification notification ->
            ( { model | notification = Just notification }, Cmd.none)

        ShowRenameInput id ->
            let
                updateEnv old =
                    { old | showRenameInput = True }

                newEnvironments =
                    List.updateIf (\elem -> elem.id == id) updateEnv model.environments

                newModel =
                    { model | environments = newEnvironments }
            in
            ( newModel, Cmd.none )

        AskRename id name ->
            let
                payload =
                    Client.UpdateEnvironment { updateEnvironmentName = name }

                newMsg =
                    Client.putApiEnvironmentByEnvironmentId "" (getCsrfToken model.session) id payload (updateEnvironmentResultToMsg id name)
            in
            ( model, newMsg )

        EnvironmentRenamed id name ->
            let
                updateEnv old =
                    { old
                        | name = NotEdited name
                        , showRenameInput = False
                    }

                mNewEnvs =
                    List.updateIf (\elem -> elem.id == id) updateEnv model.environments

                newModel =
                    { model
                        | environments = mNewEnvs
                    }
            in
            ( newModel, Cmd.none )

        AskDelete id ->
            let
                newMsg =
                    Client.deleteApiEnvironmentByEnvironmentId "" (getCsrfToken model.session) id (deleteEnvironmentResultToMsg id)
            in
            ( model, newMsg )

        EnvironmentDeleted id ->
            let
                newEnvironments =
                    List.filter (\elem -> elem.id /= id) model.environments

                newMsg =
                    case model.displayedEnvId == Just id of
                        True ->
                            Navigation.pushUrl model.navigationKey (href (EnvPage Nothing))

                        False ->
                            Cmd.none

                newModel =
                    { model
                        | environments = newEnvironments
                    }
            in
            ( newModel, newMsg )

        ChangeName id name ->
            let
                updateEnv old =
                    let
                        newName =
                            changeEditedValue name old.name
                    in
                    { old | name = newName }

                mNewEnvs =
                    List.updateIf (\elem -> elem.id == id) updateEnv model.environments

                newModel =
                    { model
                        | environments = mNewEnvs
                    }
            in
            ( newModel, Cmd.none )

        EnvironmentKeyValueEditionMsg subMsg ->
            case getEnvironmentToEdit model of
                Nothing ->
                    ( model, Cmd.none )

                Just environment ->
                    case updateKeyValue subMsg ( model, environment ) of
                         ( newModel, newEnvironment, newSubMsg ) ->
                            let
                                newEnvironments =
                                    List.updateIf (\env -> env.id == newEnvironment.id) (always newEnvironment) model.environments
                            in
                            ( { newModel | environments = newEnvironments }
                            , Cmd.map EnvironmentKeyValueEditionMsg newSubMsg
                            )


-- ** util


newEnvironmentResultToMsg : String -> Result Http.Error () -> Msg
newEnvironmentResultToMsg name result =
    case result of
        Ok () ->
            EnvironmentCreated (Debug.todo "id") name

        Err err ->
            PrintNotification <| AlertNotification "Could not create a new environment, try reloading the page!" (httpErrorToString err)


updateEnvironmentResultToMsg : Uuid -> String -> Result Http.Error () -> Msg
updateEnvironmentResultToMsg id name result =
    case result of
        Ok () ->
            EnvironmentRenamed id name

        Err err ->
            PrintNotification <| AlertNotification "Could not update environment, try reloading the page!" (httpErrorToString err)


deleteEnvironmentResultToMsg : Uuid -> Result Http.Error () -> Msg
deleteEnvironmentResultToMsg id result =
    case result of
        Ok () ->
            EnvironmentDeleted id

        Err err ->
            PrintNotification <| AlertNotification "Could not delete environment, try reloading the page!" (httpErrorToString err)


getEnvironmentToEdit : Model a -> Maybe Environment
getEnvironmentToEdit model =
    let
        selectEnvironment : Uuid -> Maybe Environment
        selectEnvironment id =
            List.find (\env -> env.id == id) model.environments
    in
    Maybe.andThen selectEnvironment model.displayedEnvId



-- ** view


view : Model a -> Element Msg
view model =
    let
        mSelectedEnv : Maybe Environment
        mSelectedEnv =
            List.find (\env -> Just env.id == model.displayedEnvId) model.environments

        keyValuesEditionView =
            case mSelectedEnv of
                Just selectedEnv ->
                    let
                        subModel =
                            { session = model.session
                            , notification = model.notification
                            , keyValues = selectedEnv.keyValues
                            , name = selectedEnv.name
                            , id = selectedEnv.id
                            }
                    in
                    el [ width fill ] <|
                        map EnvironmentKeyValueEditionMsg (envKeyValueView subModel)

                Nothing ->
                    text "no environment selected"

        envListView =
            List.map (entryView model.displayedEnvId) model.environments

        addEnvButtonView =
            Input.button [ alignLeft, width fill, Font.alignLeft ]
                { onPress = Just <| AskEnvironmentCreation "new environment"
                , label = iconWithText "note_add" "new environment"
                }
    in
    wrappedRow
        [ width fill
        , paddingXY 10 0
        , spacing 10
        ]
        [ column ( box [ alignLeft
                       , alignTop
                       , spacing 20
                       , padding 20
                       , width (fillPortion 1)
                       ]
                 )
            [ addEnvButtonView
            , column [ spacing 10 ] envListView
            ]
        , el [ width (fillPortion 9)
             , centerX
             , alignTop
             ] <|
            el ( box [ centerX, alignTop, padding 20 ] ) keyValuesEditionView
        ]


entryView : Maybe Uuid -> Environment -> Element Msg
entryView mSelectedEnvId environment =
    let
        selected =
            mSelectedEnvId == Just environment.id

        color =
            case selected  of
                True -> primaryColor
                False -> secondaryColor

        weight =
            case selected of
               True -> Font.heavy
               False -> Font.regular

        readView =
            link [ weight ] { url = href (EnvPage (Just (Debug.todo "")))--environment.id))
                    , label = el [] <| iconWithTextAndColor "label" (editedOrNotEditedValue environment.name) color
                    }

        editView =
            Input.text [ Util.onEnterWithInput (AskRename environment.id) ]
                { onChange = ChangeName environment.id
                , text = editedOrNotEditedValue environment.name
                , placeholder = Just <| Input.placeholder [] (text "environment name")
                , label = Input.labelHidden "renameRequest environment"
                }

        modeView =
            case environment.showRenameInput of
                True ->
                    editView

                False ->
                    readView
    in
    row [ spacing 5 ]
        [ modeView
        , Input.button []
            { onPress = Just <| ShowRenameInput environment.id
            , label = editIcon
            }
        , Input.button []
            { onPress = Just <| AskDelete environment.id
            , label = el [] deleteIcon
            }
        ]


labelInputView : String -> Input.Label Msg
labelInputView labelText =
    let
        size =
            width
                (fill
                    |> maximum 100
                    |> minimum 100
                )
    in
    Input.labelAbove [ centerY, size ] <| text labelText



-- *  environment key value edition
-- ** model


type alias KeyValueModel a =
    { a
        | keyValues : List (Storable NewKeyValue KeyValue)
        , notification : Maybe Notification
        , name : Editable String
        , id : Uuid
    }


newDefaultKeyValue : Storable NewKeyValue KeyValue
newDefaultKeyValue =
    New { key = ""
        , value = [ Sentence "" ]
        , hidden = False
        }



-- ** message


type KeyValueMsg
    = PromptKey Uuid String
    | PromptValue Uuid String
    | AddNewInput
    | AskSave
    | KeyValuesUpserted (List (Storable NewKeyValue KeyValue))
    | AskDeleteKeyValue Uuid
    | KeyDeleted Uuid
    | DeleteNewKeyValue Uuid
    | PrintNotification2 Notification
    | ToggleValueVisibility Uuid


-- ** update


updateKeyValue : KeyValueMsg -> ( Model a, Environment ) -> ( Model a, Environment, Cmd KeyValueMsg )
updateKeyValue msg ( model, environment ) =
    case msg of
        PromptKey id newKey ->
            let
                newKeyValues =
                    List.updateIf (\current -> True) (changeKey newKey) environment.keyValues

                newEnvironment =
                    { environment | keyValues = newKeyValues }
            in
            ( model, newEnvironment, Cmd.none )

        PromptValue idx newValue ->
            let
                newKeyValues =
                    List.updateIf (\current -> True) (changeValue (stringToTemplate newValue)) environment.keyValues

                newEnvironment =
                    { environment | keyValues = newKeyValues }
            in
            ( model, newEnvironment, Cmd.none )

        AddNewInput ->
            let
                newKeyValues =
                    environment.keyValues ++ [ newDefaultKeyValue ]

                newEnvironment =
                    { environment | keyValues = newKeyValues }
            in
            ( model, newEnvironment, Cmd.none )

        DeleteNewKeyValue idx ->
            let
                newKeyValues =
                    List.filter (always True) environment.keyValues

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

        KeyDeleted id ->
            let
                newKeyValues =
                    removeKeyValueWithId id environment.keyValues

                newEnvironment =
                    { environment | keyValues = newKeyValues }
            in
            ( model, newEnvironment, Cmd.none )

        AskSave ->
            let
                updateKeyValues =
                    List.map Client.convertEnvironmentKeyValueFromFrontToBack environment.keyValues

                newMsg =
                    Client.putApiEnvironmentByEnvironmentIdKeyValue "" (getCsrfToken model.session) environment.id updateKeyValues updateKeyValuesResultToMsg
            in
            ( model, environment, newMsg )

        KeyValuesUpserted newKeyValues ->
            let
                newEnvironment =
                    { environment | keyValues = newKeyValues }
            in
            ( model, newEnvironment, Cmd.none )

        PrintNotification2 notification ->
            ( { model | notification = Just notification }
            , environment
            , Cmd.none
            )

        ToggleValueVisibility id ->
            let
                newKeyValues =
                    List.updateIf (always True) toggleValueVisibility environment.keyValues

                newEnvironment =
                    { environment | keyValues = newKeyValues }
            in
            ( model, newEnvironment, Cmd.none )



-- ** util


updateKeyValuesResultToMsg : Result Http.Error () -> KeyValueMsg
updateKeyValuesResultToMsg result =
    case result of
        Ok () ->
            KeyValuesUpserted (Debug.todo "") --keyValues

        Err err ->
            PrintNotification2 <| AlertNotification "Could not update key value, try reloading the page!" (httpErrorToString err)


deleteKeyValueResultToMsg : Uuid -> Result Http.Error () -> KeyValueMsg
deleteKeyValueResultToMsg id result =
    case result of
        Ok () ->
            KeyDeleted id

        Err err ->
            PrintNotification2 <| AlertNotification "Could not delete key value, try reloading the page" (httpErrorToString err)


removeKeyValueWithId : Uuid -> List (Storable NewKeyValue KeyValue) -> List (Storable NewKeyValue KeyValue)
removeKeyValueWithId id keyValues =
    let
        fold : Storable NewKeyValue KeyValue -> List (Storable NewKeyValue KeyValue) -> List (Storable NewKeyValue KeyValue)
        fold storable acc =
            case storable of
                New _ ->
                    acc ++ [ storable ]

                Saved saved ->
                    case saved.id == id of
                        True ->
                            acc

                        False ->
                            acc ++ [ storable ]

                Edited2 saved _ ->
                    case saved.id == id of
                        True ->
                            acc

                        False ->
                            acc ++ [ storable ]
    in
    List.foldl fold [] keyValues


changeKey : String -> Storable NewKeyValue KeyValue -> Storable NewKeyValue KeyValue
changeKey newKey storable =
    case storable of
        New new ->
            New { new | key = newKey }

        Saved saved ->
            case saved.key == newKey of
                True ->
                    Saved saved

                False ->
                    Edited2 saved { saved | key = newKey }

        Edited2 saved edited ->
            case saved.key == newKey of
                True ->
                    Saved saved

                False ->
                    Edited2 saved { edited | key = newKey }


changeValue : StringTemplate -> Storable NewKeyValue KeyValue -> Storable NewKeyValue KeyValue
changeValue newValue storable =
    case storable of
        New new ->
            New { new | value = newValue }

        Saved saved ->
            case saved.value == newValue of
                True ->
                    Saved saved

                False ->
                    Edited2 saved { saved | value = newValue }

        Edited2 saved edited ->
            case saved.value == newValue of
                True ->
                    Saved saved

                False ->
                    Edited2 saved { edited | value = newValue }

toggleValueVisibility : Storable NewKeyValue KeyValue -> Storable NewKeyValue KeyValue
toggleValueVisibility storable =
    case storable of
        New new ->
            New { new | hidden = not new.hidden }

        Saved saved ->
            Edited2 saved { saved | hidden = not saved.hidden }

        Edited2 saved edited ->
            Edited2 saved { edited | hidden = not edited.hidden }

-- ** view


envKeyValueView : KeyValueModel a -> Element KeyValueMsg
envKeyValueView model =
    let
        addNewKeyValueView =
            Input.button []
                { onPress = Just <| AddNewInput
                , label =
                    row []
                        [ addIcon
                        , el [] (text "Add key value")
                        ]
                }
    in
    column [ spacing 30 ]
        [ titleView model
        , column [ spacing 5 ] (List.map (viewKeyValue (Debug.todo "id")) model.keyValues)
        , el [ centerX ] addNewKeyValueView
        ]


-- *** title view


titleView : KeyValueModel a -> Element KeyValueMsg
titleView model =
    let
        isModelDirty =
            List.any isStorableDirty model.keyValues

        name =
            case isModelDirty of
                True ->
                    editedOrNotEditedValue model.name ++ "*"

                False ->
                    editedOrNotEditedValue model.name
    in
    row [ centerX, paddingXY 0 10, spacing 10 ]
        [ el [] <| iconWithTextAndColor "label" name secondaryColor
        , case isModelDirty of
            True ->
                mainActionButtonsView

            False ->
                none
        ]


-- *** main action view


mainActionButtonsView : Element KeyValueMsg
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
        , label = el [ centerY ] <| iconWithTextAndColor "save" "Save" primaryColor
        }


-- *** key value entry view


viewKeyValue : Uuid -> Storable NewKeyValue KeyValue -> Element KeyValueMsg
viewKeyValue idx sKeyValue =
    let
        { key, value, hidden } =
            toLatestKeyValue sKeyValue
    in
    wrappedRow [ spacing 20 ]
        [ Input.text [ centerY ]
              { onChange = PromptKey idx
              , text = key
              , placeholder = Just <| Input.placeholder [ centerY ] (text "key")
              , label = Input.labelLeft [ centerY ] (text "Key: ")
              }
        , case hidden of
              False ->
                  Input.text [ centerY, width (px 250) ]
                      { onChange = PromptValue idx
                      , text = templatedStringAsString value
                      , placeholder = Just <| Input.placeholder [ centerY ] (text "key")
                      , label = Input.labelLeft [ centerY ] (text "Value: ")
                      }

              True ->
                  Input.newPassword [ centerY, width (px 250) ]
                      { onChange = PromptValue idx
                      , text = templatedStringAsString value
                      , placeholder = Just <| Input.placeholder [ centerY ] (text "value")
                      , label = Input.labelLeft [ centerY ] (text "Value: ")
                      , show = False
                      }
        , deleteView idx sKeyValue
        , hideView idx sKeyValue
        ]


-- *** delete view


deleteView : Uuid -> Storable NewKeyValue KeyValue -> Element KeyValueMsg
deleteView idx sKeyValue =
    case sKeyValue of
        New new ->
            Input.button []
                { onPress = Just <| DeleteNewKeyValue idx
                , label = el [] deleteIcon
                }

        Saved saved ->
            Input.button []
                { onPress = Just <| AskDeleteKeyValue saved.id
                , label = el [] deleteIcon
                }

        Edited2 saved _ ->
            Input.button []
                { onPress = Just <| AskDeleteKeyValue saved.id
                , label = el [] deleteIcon
                }


-- *** hide view


hideView : Uuid -> Storable NewKeyValue KeyValue -> Element KeyValueMsg
hideView idx sKeyValue =
    let
        label =
            case (toLatestKeyValue sKeyValue).hidden of
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
        { onPress = Just <| ToggleValueVisibility idx
        , label = label
        }
