module EnvironmentEdition.App exposing (..)

import Api.Converter as Client
import Api.WebGeneratedClient as Client
import Application.Type exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Http
import List.Extra as List
import Util exposing (..)
import StringTemplate exposing(..)


-- *  environment edition


-- ** model


type alias Model a =
    { a
        | environments : List Environment
        , session : Session
        , selectedEnvironmentToEditId : Maybe Int
    }


defaultEnvironment : Environment
defaultEnvironment =
    { id = 0
    , name = NotEdited "new environment"
    , showRenameInput = False
    , keyValues = []
    }



-- ** message


type Msg
    = SelectEnvToEdit Int
    | EnvironmentKeyValueEditionMsg KeyValueMsg
    | AskEnvironmentCreation String
    | EnvironmentCreated Int String
    | ChangeName Int String
    | AskRename Int String
    | EnvironmentRenamed Int String
    | AskDelete Int
    | EnvironmentDeleted Int
    | EnvServerError
    | ShowRenameInput Int



-- ** update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        SelectEnvToEdit id ->
            let
                newModel =
                    { model | selectedEnvironmentToEditId = Just id }
            in
            ( newModel, Cmd.none )

        AskEnvironmentCreation name ->
            let
                payload =
                    Client.NewEnvironment { newEnvironmentName = name }

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

        EnvServerError ->
            Debug.todo "server error :-("

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

                newSelectedEnvironmentToEditId =
                    case model.selectedEnvironmentToEditId == Just id of
                        True ->
                            Nothing

                        False ->
                            model.selectedEnvironmentToEditId

                newModel =
                    { model
                        | selectedEnvironmentToEditId = newSelectedEnvironmentToEditId
                        , environments = newEnvironments
                    }
            in
            ( newModel, Cmd.none )

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
                    case ( updateKeyValue subMsg ( model.session, environment ), model.selectedEnvironmentToEditId ) of
                        ( ( newEnvironment, newSubMsg ), Just id ) ->
                            let
                                newEnvironments =
                                    List.updateIf (\env -> env.id == id) (always newEnvironment) model.environments

                                newModel =
                                    { model | environments = newEnvironments }
                            in
                            ( newModel, Cmd.map EnvironmentKeyValueEditionMsg newSubMsg )

                        _ ->
                            Debug.todo "error when trying to edit environment key value"



-- ** util


newEnvironmentResultToMsg : String -> Result Http.Error Int -> Msg
newEnvironmentResultToMsg name result =
    case result of
        Ok id ->
            EnvironmentCreated id name

        Err _ ->
            EnvServerError


updateEnvironmentResultToMsg : Int -> String -> Result Http.Error () -> Msg
updateEnvironmentResultToMsg id name result =
    case result of
        Ok () ->
            EnvironmentRenamed id name

        Err _ ->
            Debug.todo "server error" EnvServerError


deleteEnvironmentResultToMsg : Int -> Result Http.Error () -> Msg
deleteEnvironmentResultToMsg id result =
    case result of
        Ok () ->
            EnvironmentDeleted id

        Err _ ->
            Debug.todo "server error" EnvServerError


getEnvironmentToEdit : Model a -> Maybe Environment
getEnvironmentToEdit model =
    let
        selectEnvironment : Int -> Maybe Environment
        selectEnvironment id =
            List.find (\env -> env.id == id) model.environments
    in
    Maybe.andThen selectEnvironment model.selectedEnvironmentToEditId



-- ** view


view : Model a -> Element Msg
view model =
    let
        mSelectedEnv : Maybe Environment
        mSelectedEnv =
            List.find (\env -> Just env.id == model.selectedEnvironmentToEditId) model.environments

        keyValuesEditionView =
            case mSelectedEnv of
                Just selectedEnv ->
                    let
                        subModel =
                            { session = model.session
                            , keyValues = selectedEnv.keyValues
                            , name = selectedEnv.name
                            , id = selectedEnv.id
                            }
                    in
                    el [] <|
                        map EnvironmentKeyValueEditionMsg (envKeyValueView subModel)

                Nothing ->
                    text "no environment selected"

        envListView =
            List.map (entryView model.selectedEnvironmentToEditId) model.environments

        addEnvButtonView =
            Input.button []
                { onPress = Just <| AskEnvironmentCreation "new environment"
                , label =
                    row []
                        [ addIcon
                        , el [] (text "Add environment")
                        ]
                }
    in
    wrappedRow
        [ width fill
        , paddingXY 10 0
        , spacing 10
        ]
        [ column
            [ alignLeft
            , alignTop
            , spacing 10
            , padding 20
            , width (fillPortion 1)
            , Background.color white
            , boxShadow
            ]
            [ column [ spacing 10 ] envListView
            , el [ centerX ] addEnvButtonView
            ]
        , el [ width (fillPortion 9)
             , centerX, alignTop
             ]
              <| el [ centerX
                    , alignTop
                    , padding 20
                    , Background.color white
                    , boxShadow
                    ] keyValuesEditionView
        ]


entryView : Maybe Int -> Environment -> Element Msg
entryView mSelectedEnvId environment =
    let
        readView =
            Input.button []
                { onPress = Just <| SelectEnvToEdit environment.id
                , label = el [] <| iconWithTextAndColor "label" (editedOrNotEditedValue environment.name) secondaryColor
                }

        editView =
            Input.text [ Util.onEnterWithInput (AskRename environment.id) ]
                { onChange = ChangeName environment.id
                , text = editedOrNotEditedValue environment.name
                , placeholder = Just <| Input.placeholder [] (text "environment name")
                , label = Input.labelHidden "rename environment"
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
        , name : Editable String
        , id : Int
    }


newDefaultKeyValue : Storable NewKeyValue KeyValue
newDefaultKeyValue =
    New { key = "", value = [Sentence ""] }



-- ** message


type KeyValueMsg
    = PromptKey Int String
    | PromptValue Int String
    | AddNewInput
    | AskSave
    | KeyValuesUpserted (List (Storable NewKeyValue KeyValue))
    | AskDeleteKeyValue Int
    | KeyDeleted Int
    | DeleteNewKeyValue Int
    | KeyValueServerError



-- ** update


updateKeyValue : KeyValueMsg -> ( Session, Environment ) -> ( Environment, Cmd KeyValueMsg )
updateKeyValue msg ( session, model ) =
    case msg of
        PromptKey idx newKey ->
            let
                newKeyValues =
                    List.updateAt idx (changeKey newKey) model.keyValues

                newModel =
                    { model | keyValues = newKeyValues }
            in
            ( newModel, Cmd.none )

        PromptValue idx newValue ->
            let
                newKeyValues =
                    List.updateAt idx (changeValue (stringToTemplate newValue)) model.keyValues

                newModel =
                    { model | keyValues = newKeyValues }
            in
            ( newModel, Cmd.none )

        AddNewInput ->
            let
                newKeyValues =
                    model.keyValues ++ [ newDefaultKeyValue ]

                newModel =
                    { model | keyValues = newKeyValues }
            in
            ( newModel, Cmd.none )

        DeleteNewKeyValue idx ->
            let
                newKeyValues =
                    List.removeAt idx model.keyValues

                newModel =
                    { model | keyValues = newKeyValues }
            in
            ( newModel, Cmd.none )

        AskDeleteKeyValue id ->
            let
                newMsg =
                    Client.deleteApiEnvironmentByEnvironmentIdKeyValueByKeyValueId "" (getCsrfToken session) model.id id (deleteKeyValueResultToMsg id)
            in
            ( model, newMsg )

        KeyDeleted id ->
            let
                newKeyValues =
                    removeKeyValueWithId id model.keyValues

                newModel =
                    { model | keyValues = newKeyValues }
            in
            ( newModel, Cmd.none )

        AskSave ->
            let
                updateKeyValues =
                    List.map Client.convertEnvironmentKeyValueFromFrontToBack model.keyValues

                newMsg =
                    Client.putApiEnvironmentByEnvironmentIdKeyValue "" (getCsrfToken session) model.id updateKeyValues updateKeyValuesResultToMsg
            in
            ( model, newMsg )

        KeyValuesUpserted newKeyValues ->
            let
                newModel =
                    { model | keyValues = newKeyValues }
            in
            ( newModel, Cmd.none )

        KeyValueServerError ->
            Debug.todo "server error while handling key values"



-- ** util


updateKeyValuesResultToMsg : Result Http.Error (List Client.KeyValue) -> KeyValueMsg
updateKeyValuesResultToMsg result =
    case result of
        Ok backKeyValues ->
            let
                keyValues =
                    List.map Client.convertEnvironmentKeyValueFromBackToFront backKeyValues
            in
            KeyValuesUpserted keyValues

        Err _ ->
            Debug.todo "server error" KeyValueServerError


deleteKeyValueResultToMsg : Int -> Result Http.Error () -> KeyValueMsg
deleteKeyValueResultToMsg id result =
    case result of
        Ok () ->
            KeyDeleted id

        Err _ ->
            Debug.todo "server error" KeyValueServerError


removeKeyValueWithId : Int -> List (Storable NewKeyValue KeyValue) -> List (Storable NewKeyValue KeyValue)
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
    column [ spacing 10 ]
        [ titleView model
        , column [ spacing 5 ] (List.indexedMap viewKeyValue model.keyValues)
        , el [ centerX ] addNewKeyValueView
        ]


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


viewKeyValue : Int -> Storable NewKeyValue KeyValue -> Element KeyValueMsg
viewKeyValue idx sKeyValue =
    let
        deleteView =
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
    in
    row [ spacing 5 ]
        [ Input.text []
            { onChange = PromptKey idx
            , text =
                case sKeyValue of
                    New { key } ->
                        key

                    Saved { key } ->
                        key

                    Edited2 _ { key } ->
                        key
            , placeholder = Just <| Input.placeholder [] (text "key")
            , label = Input.labelHidden "Key: "
            }
        , Input.text []
            { onChange = PromptValue idx
            , text =
                case sKeyValue of
                    New { value } ->
                        templatedStringAsString value

                    Saved { value } ->
                        templatedStringAsString value

                    Edited2 _ { value } ->
                        templatedStringAsString value
            , placeholder = Just <| Input.placeholder [] (text "value")
            , label = Input.labelHidden "Value: "
            }
        , deleteView
        ]
