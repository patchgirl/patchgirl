module ScenarioBuilderApp.ScenarioBuilder.Run.App exposing (..)

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
import Modal exposing (Modal(..))
import RequestComputation exposing(..)
import RequestBuilderApp.RequestBuilder.ResponseView exposing(..)
import PGBuilderApp.PGBuilder.Run.App as PgBuilder
import Parser as P
import TangoScript.Parser exposing (..)
import Set exposing (Set)
import Process
import StringTemplate exposing (..)


-- * model


type alias Model a =
    { a
        | session : Session
        , notification : Maybe Notification
        , requestCollection : RequestCollection
        , pgCollection : PgCollection
        , pgNewNode : NewNode
        , displayedPgBuilderView : BuilderView Uuid
        , scenarioCollection : ScenarioCollection
        , scenarioNewNode : NewNode
        , displayedScenarioBuilderView : RichBuilderView Uuid SceneDetailView
        , navigationKey : Navigation.Key
        , environments : List Environment
        , selectedEnvironmentToRunId : Maybe Uuid
        , runnerRunning : Bool
        , page : Page
        , whichModal : Maybe Modal
        , isLoading : List Loader
    }


-- * msg



type Msg
    -- create scene
    = ShowSceneSelectionModal (Maybe Uuid)
    | GenerateRandomUUIDForScene (Maybe Uuid) Uuid ActorType
    | AskCreateScene (Maybe Uuid) Uuid ActorType Uuid
    | AddHttpFile (Maybe Uuid) Uuid Uuid
    | AddPgFile (Maybe Uuid) Uuid Uuid
      -- delete scene
    | AskDeleteScene Uuid
    | DeleteScene Uuid
      -- update scene
    | AskUpdateSceneScript Scene
    | SceneScriptUpdated Scene
      -- change scene variables
    | ChangeSceneVariable Scene String String
    | AskUpdateSceneVariables Scene
    | SceneVariablesUpdated Scene
    | OverrideVariable Scene String Bool
      -- scenario
    | AskSaveScenario (Maybe Uuid)
    | UpdateScenarioFile (Maybe Uuid)
    | AskRunScenario
    | ScenarioProcessed ScenarioOutput
      -- detailed view
    | ShowBodyResponseView
    | ShowHeaderResponseView
    -- script
    | SetPrescript Scene String
    | SetPostscript Scene String
      -- other
    | SetEnvironmentId (Maybe Uuid)
    | PrintNotification Notification
    | DoNothing


-- * update


update : Msg -> Model a -> ScenarioFileRecord -> SceneDetailView -> (Model a, ScenarioFileRecord, Cmd Msg)
update msg model file sceneDetailView =
    case msg of


-- ** create scene


        ShowSceneSelectionModal sceneParentId ->
            let
                newModel =
                    { model | whichModal = Just (SelectNewSceneModal sceneParentId) }
            in
                ( newModel, file, Cmd.none )

        GenerateRandomUUIDForScene sceneParentId fileNodeId actorType ->
            let
                newMsg =
                    Random.generate (AskCreateScene sceneParentId fileNodeId actorType) Uuid.uuidGenerator
            in
            ( model, file, newMsg )

        AskCreateScene sceneParentId fileNodeId actorType newSceneId ->
            let
                payload =
                    { newSceneId = newSceneId
                    , newSceneSceneActorParentId = sceneParentId
                    , newSceneActorId = fileNodeId
                    , newSceneActorType = Client.convertActorTypeFromFrontToBack actorType
                    , newSceneVariables = Dict.empty
                    , newScenePrescript = ""
                    , newScenePostscript = ""
                    }

                newModel =
                    { model | isLoading = (SceneCreationPending sceneParentId) :: model.isLoading }

                newMsg =
                    Cmd.batch [ Client.postApiScenarioNodeByScenarioNodeIdScene "" (getCsrfToken model.session) file.id payload (createSceneResultToMsg sceneParentId fileNodeId newSceneId actorType)
                              , Navigation.pushUrl model.navigationKey (href (ScenarioPage (RichRunView file.id NoSceneDetailView)))
                              ]
            in
            ( newModel, file, newMsg )

        AddHttpFile sceneParentId nodeId newSceneId ->
            let
                newScene =
                    mkDefaultScene newSceneId nodeId HttpActor

                newScenes =
                    case sceneParentId of
                        Nothing ->
                            file.scenes ++ [ newScene ]

                        Just parentId ->
                            addToListAfterPredicate file.scenes (\scene -> scene.id == parentId) newScene

                newMsg =
                    Navigation.pushUrl model.navigationKey (href (ScenarioPage (RichRunView file.id (ShowDetailView newSceneId))))

                newModel =
                    { model
                        | whichModal = Nothing
                        , isLoading = List.remove (SceneCreationPending sceneParentId) model.isLoading
                    }

                newFile =
                    { file | scenes = newScenes }

            in
            ( newModel, newFile, newMsg )

        AddPgFile sceneParentId nodeId newSceneId ->
            let
                newScene =
                    mkDefaultScene newSceneId nodeId PgActor

                newScenes =
                    case sceneParentId of
                        Nothing ->
                            file.scenes ++ [ newScene ]

                        Just parentId ->
                            addToListAfterPredicate file.scenes (\scene -> scene.id == parentId) newScene

                newMsg =
                    Navigation.pushUrl model.navigationKey (href (ScenarioPage (RichRunView file.id (ShowDetailView newSceneId))))

                newModel =
                    { model
                        | whichModal = Nothing
                        , isLoading = List.remove (SceneCreationPending sceneParentId) model.isLoading
                    }

                newFile =
                    { file | scenes = newScenes }
            in
            ( newModel, newFile, newMsg )


-- ** delete scene


        AskDeleteScene sceneId ->
            let
                newMsg =
                    Client.deleteApiScenarioNodeByScenarioNodeIdSceneBySceneId "" (getCsrfToken model.session) file.id sceneId (deleteSceneResultToMsg sceneId)

                newModel =
                    { model | isLoading = SceneDeletionPending sceneId :: model.isLoading }
            in
            ( newModel, file, newMsg )

        DeleteScene deletedSceneId ->
            let
                newScenes =
                    List.filter (not << (\scene -> scene.id == deletedSceneId)) file.scenes

                newFile =
                    { file | scenes = newScenes }

                noSceneDetailMsg =
                    Navigation.pushUrl model.navigationKey (href (ScenarioPage (RichRunView file.id NoSceneDetailView)))

                newModel =
                    { model | isLoading = List.remove (SceneDeletionPending deletedSceneId) model.isLoading }

                newMsg =
                    case sceneDetailView of
                        ShowDetailView sceneId ->
                            case sceneId == deletedSceneId of
                                True ->
                                    noSceneDetailMsg

                                False ->
                                    Cmd.none

                        AddNewSceneView sceneId ->
                            case sceneId == deletedSceneId of
                                True ->
                                    noSceneDetailMsg

                                False ->
                                    Cmd.none

                        NoSceneDetailView ->
                            Cmd.none
            in
            ( newModel, newFile, newMsg )


-- ** update scene


        AskUpdateSceneScript scene ->
            let
                payload =
                    { updateSceneVariables = notEditedValue scene.variables |> Dict.map (\_ { value } -> value )
                    , updateScenePrescript = editedOrNotEditedValue scene.prescriptStr
                    , updateScenePostscript = editedOrNotEditedValue scene.postscriptStr
                    }

                newMsg =
                    Client.putApiScenarioNodeByScenarioNodeIdSceneBySceneId "" (getCsrfToken model.session) file.id scene.id payload (updateSceneResultToMsg scene)
            in
            ( model, file, newMsg )

        SceneScriptUpdated updatedScene ->
            let
                newScene =
                    { updatedScene
                        | prescriptStr = NotEdited <| editedOrNotEditedValue updatedScene.prescriptStr
                        , postscriptStr = NotEdited <| editedOrNotEditedValue updatedScene.postscriptStr
                    }

                newScenes =
                    List.updateIf (\scene -> scene.id == updatedScene.id) (always newScene) file.scenes

                newFile =
                    { file | scenes = newScenes }
            in
            ( model, newFile, Cmd.none )


-- ** change scene variable


        OverrideVariable scene key enabled ->
            let
                oldVariables =
                    editedOrNotEditedValue scene.variables

                oldValue =
                    Dict.get key oldVariables
                        |> Maybe.map .value
                        |> Maybe.withDefault ""

                newVariables =
                    oldVariables
                        |> Dict.insert key { value = oldValue
                                           , enabled = enabled
                                           }

                newScene =
                    { scene
                        | variables = changeEditedValue newVariables scene.variables
                    }

                newScenes =
                    List.updateIf (\s -> s.id == scene.id) (always newScene) file.scenes

                newFile =
                    { file | scenes = newScenes }
            in
            ( model, newFile, Cmd.none )

        ChangeSceneVariable scene key newVar ->
            let
                oldVariables =
                    editedOrNotEditedValue scene.variables

                newValue =
                    case Dict.get key oldVariables of
                        Nothing ->
                            { enabled = False
                            , value = newVar
                            }

                        Just definedValue ->
                            { definedValue | value = newVar }

                newVariables =
                    oldVariables
                        |> Dict.insert key newValue

                newScene =
                    { scene
                        | variables = changeEditedValue newVariables scene.variables
                    }

                newScenes =
                    List.updateIf (\s -> s.id == scene.id) (always newScene) file.scenes

                newFile =
                    { file | scenes = newScenes }
            in
            ( model, newFile, Cmd.none )

        AskUpdateSceneVariables scene ->
            let
                payload =
                    { updateSceneVariables = editedOrNotEditedValue scene.variables |> Dict.map (\_ { value } -> value )
                    , updateScenePrescript = notEditedValue scene.prescriptStr
                    , updateScenePostscript = notEditedValue scene.postscriptStr
                    }

                newMsg =
                    Client.putApiScenarioNodeByScenarioNodeIdSceneBySceneId "" (getCsrfToken model.session) file.id scene.id payload (updateSceneVariablesResultToMsg scene)
            in
            ( model, file, newMsg )

        SceneVariablesUpdated updatedScene ->
            let
                newScene =
                    { updatedScene
                        | variables = NotEdited <| editedOrNotEditedValue updatedScene.variables
                    }

                newScenes =
                    List.updateIf (\scene -> scene.id == updatedScene.id) (always newScene) file.scenes

                newFile =
                    { file | scenes = newScenes }
            in
            ( model, newFile, Cmd.none )


-- ** scenario


        AskSaveScenario newEnvironmentId ->
            let
                (ScenarioCollection scenarioId _) =
                    model.scenarioCollection

                payload =
                    { updateScenarioFileId = file.id
                    , updateScenarioFileEnvironmentId = newEnvironmentId
                    }

                newMsg =
                    Client.putApiScenarioCollectionByScenarioCollectionIdScenarioFile "" (getCsrfToken model.session) scenarioId payload (updateScenarioResultToMsg newEnvironmentId)
            in
            (model, file, newMsg)

        UpdateScenarioFile newEnvironmentId ->
            let
                newFile =
                    { file | environmentId = NotEdited newEnvironmentId }
            in
            (model, newFile, Cmd.none)

        AskRunScenario ->
            let
                sceneToSceneInput : Scene -> Maybe Client.SceneFile
                sceneToSceneInput scene =
                    case (scene.prescriptAst, scene.postscriptAst, findRecord model scene) of
                        (Ok prescript, Ok postscript, Just record) ->
                            case record of
                                HttpRecord httpRecord ->
                                    buildRequestComputationInput httpRecord
                                        |> \requestComputationInput ->
                                            Just <| Client.HttpSceneFile { sceneId = scene.id
                                                                         , sceneFileId = scene.nodeId
                                                                         , sceneHttpInput =
                                                                               (Client.convertRequestComputationInputFromFrontToBack requestComputationInput)
                                                                         , scenePrescript =
                                                                               Client.convertTangoscriptFromFrontToBack prescript
                                                                         , scenePostscript =
                                                                             Client.convertTangoscriptFromFrontToBack postscript
                                                                         }

                                PgRecord pgRecord ->
                                    PgBuilder.buildPgComputationPayload pgRecord model
                                        |> Tuple.second
                                        |> \pgComputationInput ->
                                           Just <| Client.PgSceneFile { sceneId = scene.id
                                                                      , sceneFileId = scene.nodeId
                                                                      , scenePrescript =
                                                                          Client.convertTangoscriptFromFrontToBack prescript
                                                                      , scenePostscript =
                                                                          Client.convertTangoscriptFromFrontToBack postscript
                                                                      , scenePgInput = pgComputationInput
                                                                      }

                        _ -> Nothing

                mScenes : Maybe (List Client.SceneFile)
                mScenes =
                    let
                        a =
                            file.scenes
                                |> List.map sceneToSceneInput
                    in
                        a |> traverseListMaybe

                mPayload =
                    mScenes
                        |> Maybe.map
                           (\scenes ->
                                { scenarioInputId = file.id
                                , scenarioInputScenes = scenes
                                , scenarioInputEnvVars =
                                    environmentKeyValues model file
                                        |> Dict.map ( \_ value -> Client.convertStringTemplateFromFrontToBack value )
                                }
                           )

                newMsg =
                    case mPayload of
                        Just payload ->
                            Client.postApiRunnerScenarioComputation Runner.desktopRunnerUrl payload runScenarioResultToMsg

                        Nothing ->
                            Cmd.none

                resetScenes =
                    file.scenes
                        |> List.map (\scene -> { scene | sceneComputation = Nothing })

                newFile =
                    { file | scenes = resetScenes }

            in
            ( model, newFile, newMsg )

        ScenarioProcessed scenarioOutput ->
            let
                mergeSceneComputationOutputResult : Scene -> Scene
                mergeSceneComputationOutputResult scene =
                    let
                        newSceneComputation =
                            List.find (\s -> s.sceneId == scene.id) scenarioOutput
                                |> Maybe.map .sceneComputation
                    in
                    { scene | sceneComputation = newSceneComputation }

                newScenes =
                    List.map mergeSceneComputationOutputResult file.scenes

                newFile =
                    { file | scenes = newScenes }
            in
            ( model, newFile, Cmd.none )


-- ** detailed view

        ShowBodyResponseView ->
--            let
--                newFile =
--                    { file | whichResponseView = BodyResponseView }
--            in
            ( model, file, Cmd.none )

        ShowHeaderResponseView ->
--            let
--                newFile =
--                    { file | whichResponseView = HeaderResponseView }
--            in
            ( model, file, Cmd.none )


-- ** script


        SetPrescript scene newScriptStr ->
            let
                newScene =
                    { scene
                        | prescriptStr = changeEditedValue newScriptStr scene.prescriptStr
                        , prescriptAst = parseTangoscript newScriptStr
                    }

                newScenes =
                    List.updateIf (\s -> s.id == scene.id) (always newScene) file.scenes

                newFile =
                    { file | scenes = newScenes }
            in
            ( model, newFile, Cmd.none )

        SetPostscript scene newScriptStr ->
            let
                newScene =
                    { scene
                        | postscriptStr = changeEditedValue newScriptStr scene.postscriptStr
                        , postscriptAst = parseTangoscript newScriptStr
                    }

                newScenes =
                    List.updateIf (\s -> s.id == scene.id) (always newScene) file.scenes

                newFile =
                    { file | scenes = newScenes }
            in
            ( model, newFile, Cmd.none )


-- ** other


        SetEnvironmentId mEnvId ->
            let
                newFile =
                    { file | environmentId = changeEditedValue mEnvId file.environmentId }
            in
            ( model, newFile, Cmd.none )

        PrintNotification notification ->
            ( { model | notification = Just notification }, file, Cmd.none )

        DoNothing ->
            (model, file, Cmd.none)


-- * util


mkDefaultScene : Uuid -> Uuid -> ActorType -> Scene
mkDefaultScene id nodeId actorType =
    { id = id
    , nodeId = nodeId
    , actorType = actorType
    , sceneComputation = Nothing
    , variables = NotEdited Dict.empty
    , prescriptStr = NotEdited ""
    , prescriptAst = Ok []
    , postscriptStr = NotEdited ""
    , postscriptAst = Ok []
    }


runScenarioResultToMsg : Result Http.Error Client.ScenarioOutput -> Msg
runScenarioResultToMsg result =
    case result of
        Ok scenarioOutput ->
            ScenarioProcessed (Client.convertScenarioOutputFromBackToFront scenarioOutput)

        Err err ->
            PrintNotification <|
                AlertNotification ("Could not run scenario. Is <a href=\"" ++ (href (DocumentationPage PatchGirlRunnerAppDoc)) ++ "\">patchgirl-runner</a> running?") (httpErrorToString err)


deleteSceneResultToMsg : Uuid -> Result Http.Error () -> Msg
deleteSceneResultToMsg sceneId result =
    case result of
        Ok () ->
            DeleteScene sceneId

        Err err ->
            PrintNotification <| AlertNotification ("Could not delete scene, try to reload the page.") (httpErrorToString err)


createSceneResultToMsg : Maybe Uuid -> Uuid -> Uuid -> ActorType -> Result Http.Error () -> Msg
createSceneResultToMsg sceneParentId nodeId newSceneId actorType result =
    case result of
        Ok () ->
            case actorType of
                HttpActor ->
                    AddHttpFile sceneParentId nodeId newSceneId

                PgActor ->
                    AddPgFile sceneParentId nodeId newSceneId

        Err err ->
            PrintNotification <| AlertNotification "Could not create the scene, try reloading the page!" (httpErrorToString err)

updateSceneResultToMsg : Scene -> Result Http.Error () -> Msg
updateSceneResultToMsg scene result =
    case result of
        Ok () ->
            SceneScriptUpdated scene

        Err err ->
            PrintNotification <| AlertNotification "Could not update the scene, try reloading the page!" (httpErrorToString err)

updateSceneVariablesResultToMsg : Scene -> Result Http.Error () -> Msg
updateSceneVariablesResultToMsg scene result =
    case result of
        Ok () ->
            SceneVariablesUpdated scene

        Err err ->
            PrintNotification <| AlertNotification "Could not update the scene, try reloading the page!" (httpErrorToString err)

updateScenarioResultToMsg : Maybe Uuid -> Result Http.Error () -> Msg
updateScenarioResultToMsg newEnvironmentId result =
    case result of
        Ok () ->
            UpdateScenarioFile newEnvironmentId

        Err err ->
            PrintNotification  <| AlertNotification "Could not update the scenario, try reloading the page!" (httpErrorToString err)

findRecord : Model a -> Scene -> Maybe FileRecord
findRecord model scene =
    let
        (RequestCollection _ requestNodes) =
            model.requestCollection

        (PgCollection _ pgNodes) =
            model.pgCollection
    in
    case scene.actorType of
        HttpActor ->
            findRequestFile requestNodes scene.nodeId |> Maybe.map HttpRecord

        PgActor ->
            findPgFile pgNodes scene.nodeId |> Maybe.map PgRecord

environmentKeyValues : Model a -> ScenarioFileRecord -> Dict String StringTemplate
environmentKeyValues model file =
    editedOrNotEditedValue file.environmentId
        |> Maybe.andThen (\scenarioEnvId -> List.find (\env -> (env.id == scenarioEnvId)) model.environments)
        |> Maybe.map .keyValues
        |> Maybe.withDefault []
        |> List.map (\{key, value} -> ( editedOrNotEditedValue key
                                      , editedOrNotEditedValue value
                                      )
                    )
        |> Dict.fromList


-- * view


view : Model a -> ScenarioFileRecord -> SceneDetailView -> Element Msg
view model file sceneDetailView =
    let
        scenesView =
            case file.scenes of
                [] ->
                    selectSceneView model Nothing

                scenes ->
                    column [ centerX, spacing 10 ] (List.map (sceneView model file sceneDetailView) scenes)

    in
    wrappedRow [ height fill, width fill, spacing 20 ]
        [ el ( box [ width <| fillPortion 1, alignTop, padding 20 ] ) (scenarioSettingView model file)
        , row [ width <| fillPortion 9, alignTop, spacing 20 ]
            [ el [ width <| fillPortion 2, height fill ] scenesView
            , el [ width <| fillPortion 8, height fill, alignRight ] <|
                case sceneDetailView of
                    ShowDetailView sceneId ->
                        detailedSceneView model file sceneId

                    AddNewSceneView sceneId ->
                        selectSceneView model (Just sceneId)

                    NoSceneDetailView ->
                        none
            ]
        ]


-- ** scenario setting view


envSelectionView : Model a -> ScenarioFileRecord -> Element Msg
envSelectionView model file =
    let
        noEnvironmentOption : Input.Option (Maybe Uuid) Msg
        noEnvironmentOption =
            Input.option Nothing <|
                el [ width fill ] (text "No environment")

        option : Environment -> Input.Option (Maybe Uuid) Msg
        option environment =
            Input.option (Just environment.id) <|
                el [ width fill ] (text <| editedOrNotEditedValue environment.name)
    in
    Input.radio [ padding 10, spacing 10, width fill ]
        { onChange = SetEnvironmentId
        , selected = Just (editedOrNotEditedValue file.environmentId)
        , label = Input.labelHidden "Scenario environment"
        , options = noEnvironmentOption :: List.map option model.environments
        }

sceneToKeyValues : Model a
                 -> ScenarioFileRecord
                 -> Scene
                 -> Maybe
                    { scene : Scene
                    , sceneName : String
                    , sceneKeys : List String
                    , sceneUserDefinedKeyValue : Dict String UserSceneVariableValue
                    }
sceneToKeyValues model file scene =
    case scene.actorType of
        PgActor ->
            Debug.todo ""

        HttpActor ->
            let
                (RequestCollection _ nodes) =
                    model.requestCollection
            in
            case findRequestNode nodes scene.nodeId of
                Just (File requestFileRecord) ->
                    ( [ editedOrNotEditedValue requestFileRecord.name
                      , editedOrNotEditedValue requestFileRecord.httpUrl
                      , editedOrNotEditedValue requestFileRecord.httpBody
                      ] ++ (editedOrNotEditedValue requestFileRecord.httpHeaders |> List.concatMap (\(headerKey, headerValue) -> [ headerKey, headerValue ] ))
                    )
                        |> List.andThen stringToTemplate
                        |> List.filterMap
                           ( \template ->
                                 case template of
                                     Sentence _ -> Nothing
                                     Key s -> Just s
                           )
                        |> \sceneKeys -> Just { scene = scene
                                              , sceneName = editedOrNotEditedValue requestFileRecord.name
                                              , sceneKeys = sceneKeys
                                              , sceneUserDefinedKeyValue = editedOrNotEditedValue scene.variables
                                              }

                _ -> Nothing

currentEnvironmentKeyValues : Model a -> ScenarioFileRecord -> Dict String String
currentEnvironmentKeyValues model file =
    environmentKeyValues model file
        |> Dict.map (\_ value -> stringTemplateToString value)

sceneFormView : Dict String String
              -> { scene : Scene
                 , sceneName : String
                 , sceneKeys : List String
                 , sceneUserDefinedKeyValue : Dict String UserSceneVariableValue
                 }
              -> Element Msg
sceneFormView scenarioKeyValues { scene, sceneName, sceneKeys, sceneUserDefinedKeyValue } =
    let
        keyValueView : String -> Element Msg
        keyValueView key =
            let
                mUserValue =
                    Dict.get key sceneUserDefinedKeyValue

                enabled =
                    mUserValue
                        |> Maybe.map .enabled
                        |> Maybe.withDefault False
            in
            row [ width fill, spacing 10 ]
                [ row [width fill ]
                      [ el [ alignLeft ] <| text (key ++ ": ")
                      , case Dict.get key scenarioKeyValues of
                            Nothing ->
                                el [ centerY
                                   , padding 10
                                   , htmlAttribute (Html.class "left-arrow-box")
                                   , Font.color redColor, Font.bold
                                   ] (text "undefined")

                            Just defaultValue ->
                                el [ centerY
                                   , padding 10
                                   , htmlAttribute (Html.class "left-arrow-box")
                                   , Font.color greenColor, Font.bold
                                   ] (text defaultValue)
                      ]
                 , row [ width fill, alignRight ]
                     [ Input.checkbox []
                           { onChange = OverrideVariable scene key
                           , icon = Input.defaultCheckbox
                           , checked = enabled
                           , label =
                               Input.labelRight [ height fill, centerY ] <|
                                   el [ centerY ] <| text "override with: "
                           }
                     , Input.text [ height fill, centerY
                                  , case enabled of
                                        True -> noAttribute
                                        False -> Background.color lightGrey
                                  ]
                           { onChange = ChangeSceneVariable scene key
                           , text = Maybe.map .value mUserValue |> Maybe.withDefault ""
                           , placeholder = Nothing
                           , label = Input.labelHidden "override value"
                           }
                     ]
                ]

    in
    column [ spacing 20, width fill ]
        [ row [ width fill ]
              [ iconWithAttr { defaultIconAttribute
                                 | title = sceneName
                                 , icon = "label"
                                 , primIconColor = Just secondaryColor
                             }
              , case isDirty scene.variables of
                    False -> none
                    True ->
                        Input.button [ alignRight
                                     , Border.solid
                                     , Border.color secondaryColor
                                     , Border.width 1
                                     , Border.rounded 5
                                     , Background.color secondaryColor
                                     , paddingXY 10 10
                                     ]
                            { onPress = Just (AskUpdateSceneVariables scene)
                            , label =
                                row [ centerX, centerY ]
                                    [ iconWithTextAndColorAndAttr "save" "Save" primaryColor []
                                    ]
                            }

              ]
        , column [ spacing 10, width fill ] <|
            List.map keyValueView sceneKeys
        ]

variablesFormView : Model a -> ScenarioFileRecord -> Element Msg
variablesFormView model file =
    let
        scenesInfo =
            List.map (sceneToKeyValues model file) file.scenes
                |> catMaybes
                |> List.filter ( \{ sceneKeys } -> (not << List.isEmpty) sceneKeys )
    in
    column [ spacing 20, width fill ] <|
        List.map (sceneFormView (currentEnvironmentKeyValues model file)) scenesInfo

saveScenarioView : ScenarioFileRecord -> Element Msg
saveScenarioView file =
    case file.environmentId of
        NotEdited _ -> none
        Edited _ environmentId ->
            Input.button [ centerX
                         , Border.solid
                         , Border.color secondaryColor
                         , Border.width 1
                         , Border.rounded 5
                         , Background.color secondaryColor
                         , paddingXY 10 10
                         ]
            { onPress = Just (AskSaveScenario environmentId)
            , label =
                row [ centerX, centerY ]
                    [ iconWithTextAndColorAndAttr "save" "Save" primaryColor []
                    ]
            }

scenarioSettingView : Model a -> ScenarioFileRecord -> Element Msg
scenarioSettingView model file =
    let
        title =
            iconWithAttr { defaultIconAttribute
                             | title = (editedOrNotEditedValue file.name)
                             , icon = "label"
                             , primIconColor = Just secondaryColor
                         }

        runButton =
            Input.button [ centerX
                         , Border.solid
                         , Border.color secondaryColor
                         , Border.width 1
                         , Border.rounded 5
                         , Background.color secondaryColor
                         , paddingXY 10 10
                         ]
                { onPress = Just AskRunScenario
                , label =
                    el [ centerX, centerY ] <|
                        iconWithAttr { defaultIconAttribute
                                     | title = "Run"
                                     , icon = "send"
                                     , primIconColor = Just primaryColor
                                     }
                }

        quitLink =
            link []
                { url = href <| ScenarioPage (RichLandingView  DefaultView)
                , label = el [] clearIcon
                }

    in
    column [ width fill, centerX, spacing 30 ]
        [ row [ spacing 10, width fill ]
              [ title
              , el [ alignRight ] (saveScenarioView file)
              , runButton
              , quitLink
              ]
        , column [ spacing 30, width fill]
            [ el [ width fill
                 , Font.center
                 , Font.size 22
                 , Font.underline
                 ] <| text "Select scenario environment"
            , envSelectionView model file
            ]
        , column [ spacing 30, width fill ]
            [ el [ width fill
                 , Font.center
                 , Font.size 22
                 , Font.underline
                 ] <| text "Override scenario variables"
            , variablesFormView model file
            ]
        ]


-- ** scene view


sceneView : Model a -> ScenarioFileRecord -> SceneDetailView -> Scene -> Element Msg
sceneView model file sceneDetailView scene =
    let
        selected =
            case sceneDetailView of
                ShowDetailView sceneId ->
                    sceneId == scene.id

                AddNewSceneView _ ->
                    False

                NoSceneDetailView ->
                    False

        selectedSceneAttrs =
            case selected of
                True ->
                    Border.color black
                False ->
                    Border.color white

        sceneComputationAttrs =
            case scene.sceneComputation of
                Nothing ->
                    [ Border.color white ]

                Just (HttpSceneOk _) ->
                    [ borderSuccess, backgroundSuccess ]

                Just (PgSceneOk _) ->
                    [ borderSuccess, backgroundSuccess ]

                Just _ ->
                    [ borderError, backgroundError ]
    in
    case findRecord model scene of
        Just fileRecord ->
            let
                { name } =
                    case fileRecord of
                        HttpRecord r -> { id = r.id, name = r.name }
                        PgRecord r -> { id = r.id, name = r.name }

                sceneIcon =
                    case scene.actorType of
                        HttpActor ->
                            "public"

                        PgActor ->
                            "storage"
            in
            column [ centerX, spacing 10 ]
                [ row
                    ( box [ Border.width 1, centerX, paddingXY 10 10 ] ++ sceneComputationAttrs ++ [ selectedSceneAttrs ]
                    ) [ link [ padding 20, width fill, height fill ]
                           { url = href <| ScenarioPage (RichRunView file.id (ShowDetailView scene.id))
                           , label =
                               row [ spacing 20, centerX ]
                                   [ row []
                                         [ iconWithAttr { defaultIconAttribute | icon = sceneIcon, title = "" }
                                         , el ( case selected of
                                                    False -> []
                                                    True -> [ Font.bold ]
                                              ) <| text (notEditedValue name)
                                         ]
                                   ]
                           }
                     , case List.member (SceneDeletionPending scene.id) model.isLoading of
                           False ->
                               Input.button []
                                   { onPress = Just (AskDeleteScene scene.id)
                                   , label = el [ alignRight ] deleteIcon
                                   }

                           True ->
                               smallLoader

                      , el [ width (px 10) ] none
                      ]
                , arrowView model sceneDetailView file.id scene.id
                ]

        _ ->
            none



-- ** arrow view


arrowView : Model a -> SceneDetailView -> Uuid -> Uuid -> Element Msg
arrowView model sceneDetailView id sceneId =
    let
        selected =
            case sceneDetailView of
                ShowDetailView _ ->
                    False

                AddNewSceneView parentSceneId ->
                    parentSceneId == sceneId

                NoSceneDetailView ->
                    False

        addNewSceneLink =
            link []
                { url = href <| ScenarioPage (RichRunView id (AddNewSceneView sceneId))
                , label = row [] [ addIcon
                                 , el ( case selected of
                                            True -> [ Font.bold ]
                                            False -> []
                                      ) <| text " add new scene"
                                 ]
                }
    in
        column [ centerX, spacing 5 ]
            [ el ( [ centerX
                   , padding 10
                   ]
                 ) <|
                  case List.member (SceneCreationPending (Just sceneId)) model.isLoading of
                      True -> smallLoader
                      False -> addNewSceneLink
            , el [ centerX ] arrowDownwardIcon
            ]



-- ** detailed scene view


detailedSceneView : Model a -> ScenarioFileRecord -> Uuid -> Element Msg
detailedSceneView model file sceneId =
    let
        mSceneAndRecord =
            List.find (\scene -> scene.id == sceneId) (file.scenes)
                |> Maybe.andThen (\scene -> (findRecord model scene) |> Maybe.map (\record -> (scene, record)))
    in
    case mSceneAndRecord of
        Nothing ->
            el ( box [ width fill
                     , height fill
                     , centerX
                     , alignTop
                     , spacing 40
                     , padding 20
                     ]
               ) none

        Just (scene, fileRecord) ->
            column ( box [ width fill
                         , height fill
                         , centerX
                         , alignTop
                         , spacing 40
                         , padding 20
                         ]
                   ) <|
                case fileRecord of
                    HttpRecord record ->
                        httpDetailedSceneView file scene record

                    PgRecord record ->
                        pgDetailedSceneView file scene record


-- *** http detailed scene view


httpDetailedSceneView : ScenarioFileRecord -> Scene -> RequestFileRecord -> List (Element Msg)
httpDetailedSceneView file scene fileRecord =
    let
        { method, url } =
            buildRequestComputationInput fileRecord

        methodAndUrl =
            (methodToString method)
            ++ " "
            ++ (stringTemplateToString url)

        saveSceneButton =
            case isDirty scene.prescriptStr || isDirty scene.postscriptStr of
                False ->
                    none

                True ->
                    Input.button [ Border.solid
                                 , Border.color secondaryColor
                                 , Border.width 1
                                 , Border.rounded 5
                                 , Background.color secondaryColor
                                 , paddingXY 10 10
                                 ]
                        { onPress = Just (AskUpdateSceneScript scene)
                        , label =
                            row [ centerX, centerY ]
                                [ iconWithTextAndColorAndAttr "save" "Save" primaryColor []
                                ]
                        }

        sceneInputDetailView =
            column [ width fill, spacing 15 ]
              [ row [ spacing 10, width fill ]
                    [ link [ alignLeft ]
                          { url = href <| ReqPage (RunView fileRecord.id)
                          , label = el [ Font.underline, Font.size 25 ] <|
                                    iconWithAttr { defaultIconAttribute
                                                     | icon = "public"
                                                     , title = (editedOrNotEditedValue fileRecord.name)
                                                     , primIconColor = Just primaryColor
                                                 }
                          }
                    , saveSceneButton
                    , link [ alignRight ]
                        { url = href <| ScenarioPage (RichRunView file.id NoSceneDetailView)
                        , label = el [ alignRight ] clearIcon
                        }
                    ]
              , Input.text [ Background.color lightGrey ]
                  { onChange = always DoNothing
                  , text = methodAndUrl
                  , placeholder = Nothing
                  , label = Input.labelHidden "http gist"
                  }
              ]

        whichResponseButtonView : List (String, Bool, Msg) -> Element Msg
        whichResponseButtonView tabs =
            let
                buttonView (label, isActive, msg) =
                    Input.button [ centerX, centerY
                                 , height fill
                                 ]
                        { onPress = Just msg
                        , label =
                            el ( [ centerY, centerX
                                 , height fill
                                 ] ++ (selectiveButtonAttrs isActive)
                               ) <| el [ centerY ] (text label)
                        }
            in
            row [ width fill, height (px 50)
                , centerX, centerY
                , spacing 20
                , paddingXY 0 0
                ] <| List.map buttonView tabs

        outputSceneDetailView : SceneComputation -> Element Msg
        outputSceneDetailView sceneComputation =
            case sceneComputation of
                SceneNotRun ->
                    text <| "This request hasn't been run"

                PrescriptFailed scriptException ->
                    text <| "Prescript failed because of: " ++ scriptExceptionToString scriptException

                HttpSceneFailed httpException ->
                    paragraph [] [ text <| "This request failed because of: " ++ httpExceptionToString httpException ]

                PgSceneFailed _ ->
                    none

                HttpPostscriptFailed _ scriptException ->
                    text <| "Postscript failed because of: " ++ scriptExceptionToString scriptException

                PgPostscriptFailed _ _ ->
                    none

                HttpSceneOk requestComputationOutput ->
                    column [ width fill ]
                        [ statusResponseView requestComputationOutput
                        , whichResponseButtonView
                              [ ("Body", fileRecord.whichResponseView == BodyResponseView, ShowBodyResponseView)
                              , ("Headers", fileRecord.whichResponseView == HeaderResponseView, ShowHeaderResponseView)
                              ]
                        , case fileRecord.whichResponseView of
                              BodyResponseView ->
                                  bodyResponseView requestComputationOutput (always DoNothing)

                              HeaderResponseView ->
                                  headersResponseView requestComputationOutput (always DoNothing)
                        ]

                PgSceneOk _ ->
                    none


    in
    case scene.sceneComputation of
        Nothing ->
            [ sceneInputDetailView
            , prescriptView scene
            , postscriptView scene
            ]

        Just sceneComputation ->
            [ sceneInputDetailView
            , prescriptView scene
            , postscriptView scene
            , hr [] "response"
            , outputSceneDetailView sceneComputation
            ]


-- *** pg detailed scene view


pgDetailedSceneView : ScenarioFileRecord -> Scene -> PgFileRecord -> List (Element Msg)
pgDetailedSceneView file scene fileRecord =
    let
        sqlText =
            editedOrNotEditedValue fileRecord.sql
                |> softEllipsis 80

        saveSceneButton =
            case isDirty scene.prescriptStr || isDirty scene.postscriptStr of
                False ->
                    none

                True ->
                    Input.button [ Border.solid
                                 , Border.color secondaryColor
                                 , Border.width 1
                                 , Border.rounded 5
                                 , Background.color secondaryColor
                                 , paddingXY 10 10
                                 ]
                        { onPress = Just (AskUpdateSceneScript scene)
                        , label =
                            row [ centerX, centerY ]
                                [ iconWithTextAndColorAndAttr "save" "Save" primaryColor []
                                ]
                        }

        sceneInputDetailView =
            column [ width fill, spacing 15 ]
              [ row [ spacing 10, width fill ]
                    [ link [ alignLeft ]
                          { url = href <| PgPage (RunView fileRecord.id)
                          , label = el [ Font.underline, Font.size 25 ] <|
                                    iconWithAttr { defaultIconAttribute
                                                     | icon = "storage"
                                                     , title = (editedOrNotEditedValue fileRecord.name)
                                                     , primIconColor = Just primaryColor
                                                 }
                          }
                    , saveSceneButton
                    , link [ alignRight ]
                        { url =  href <| ScenarioPage (RichRunView file.id NoSceneDetailView)
                        , label = el [ alignRight ] clearIcon
                        }
                    ]
              , Input.multiline [ Background.color lightGrey ]
                  { onChange = always DoNothing
                  , text = sqlText
                  , placeholder = Nothing
                  , label = Input.labelHidden "http gist"
                  , spellcheck = False
                  }
              ]

        outputSceneDetailView : SceneComputation -> Element Msg
        outputSceneDetailView sceneComputation =
            case sceneComputation of
                SceneNotRun ->
                    text <| "This request hasn't been run"

                PrescriptFailed scriptException ->
                    text <| "Prescript failed because of: " ++ scriptExceptionToString scriptException

                HttpSceneFailed _ ->
                    none

                PgSceneFailed error ->
                    PgBuilder.responseView (Err error)

                HttpPostscriptFailed _ _ ->
                    none

                PgPostscriptFailed _ scriptException ->
                    text <| "Postscript failed because of: " ++ scriptExceptionToString scriptException

                HttpSceneOk _ ->
                    none

                PgSceneOk pgComputation ->
                    PgBuilder.responseView (Ok pgComputation)
    in
    case scene.sceneComputation of
        Nothing ->
            [ sceneInputDetailView
            , prescriptView scene
            , postscriptView scene
            ]

        Just sceneComputation ->
            [ sceneInputDetailView
            , prescriptView scene
            , postscriptView scene
            , hr [] "response"
            , outputSceneDetailView sceneComputation
            ]


-- ** script view


scriptValidityView : Result (List P.DeadEnd) TangoAst -> Element Msg
scriptValidityView result =
    case result of
        Ok _ ->
            none

        Err err ->
            text ("Error in script: " ++ showErrors err)

prescriptView : Scene -> Element Msg
prescriptView scene =
    column [ width fill, spacing 10 ]
        [ el [] <| scriptValidityView scene.prescriptAst
        , Input.multiline []
              { onChange = SetPrescript scene
              , text = editedOrNotEditedValue scene.prescriptStr
              , placeholder = Just <| Input.placeholder [] (text "set(\"userId\", 1); // set variable to use in your request")
              , label = labelInputView "Prescript: "
              , spellcheck = False
              }
        ]


postscriptView : Scene -> Element Msg
postscriptView scene =
    column [ width fill, spacing 10 ]
        [ el [] <| scriptValidityView scene.postscriptAst
        , Input.multiline []
            { onChange = SetPostscript scene
            , text = editedOrNotEditedValue scene.postscriptStr
            , placeholder = Just <| Input.placeholder [] (text "assertEqual(httpResponseStatus, 200); // test the http response")
            , label = labelInputView "Postscript: "
            , spellcheck = False
            }
        ]


-- ** util


labelInputView : String -> Input.Label Msg
labelInputView labelText =
    Input.labelAbove [ centerY, width fill ]
        <| row [ width fill ]
            [ el [ alignLeft ] (text labelText)
            , link [ alignRight, Font.alignRight ]
                { url = href <| DocumentationPage ScenarioDoc
                , label = el [ Font.underline ] (rightIconWithTextAndColor "help" "Help" primaryColor)
                }
            ]


-- ** select scene view


selectSceneView : Model a -> Maybe Uuid -> Element Msg
selectSceneView model sceneParentId =
    let
        (RequestCollection _ requestNodes) =
            model.requestCollection

        (PgCollection _ pgNodes) =
            model.pgCollection

        treeView =
            row [ centerX ]
                [ column [ spacing 30, width (fillPortion 1) ]
                      [ el [ centerX, Font.size 23 ] <|
                            iconWithAttr { defaultIconAttribute
                                             | iconSize = Just "25px"
                                             , iconVerticalAlign = Just "sub"
                                             , icon = "public"
                                             , title = " HTTP"
                                         }
                      , column [ spacing 10, centerX ] (httpNodeView requestNodes)
                      ]
                , column [ spacing 30, width (fillPortion 1), alignTop ]
                      [ el [ centerX, Font.size 23 ] <|
                            iconWithAttr { defaultIconAttribute
                                             | iconSize = Just "25px"
                                             , iconVerticalAlign = Just "sub"
                                             , icon = "storage"
                                             , title = " Postgres"
                                         }
                      , column [ spacing 10, centerX ] (pgNodeView pgNodes)
                      ]
                ]

        httpNodeView : List RequestNode -> List (Element Msg)
        httpNodeView nodes =
            case nodes of
                [] ->
                    []

                node :: tail ->
                    case node of
                        Folder { id, name, open, children } ->
                            let
                                (RequestChildren c) =
                                    children

                                folderChildrenView =
                                    httpNodeView c

                                tailView =
                                    httpNodeView tail

                                currentFolderView =
                                    httpFolderView id name folderChildrenView open
                            in
                            currentFolderView :: tailView

                        File requestFileRecord ->
                            let
                                tailView =
                                    httpNodeView tail

                                currentFileView =
                                    Input.button []
                                        { onPress = Just (GenerateRandomUUIDForScene sceneParentId requestFileRecord.id HttpActor)
                                        , label = el [] <| iconWithTextAndColor "label" (notEditedValue requestFileRecord.name) secondaryColor
                                        }
                            in
                            currentFileView :: tailView

        httpFolderView : Uuid -> Editable String -> List (Element Msg) -> Bool -> Element Msg
        httpFolderView id name folderChildrenView open =
            let
                folderIcon =
                    case open of
                        False ->
                            "keyboard_arrow_right"

                        True ->
                            "keyboard_arrow_down"
            in
            column []
                [ iconWithText folderIcon (notEditedValue name)
                , case open of
                    True ->
                        column [ spacing 10, paddingXY 20 10 ] folderChildrenView

                    False ->
                        none
                ]

        pgNodeView : List PgNode -> List (Element Msg)
        pgNodeView nodes =
            case nodes of
                [] ->
                    []

                node :: tail ->
                    case node of
                        Folder { id, name, open, children } ->
                            let
                                (PgChildren c) =
                                    children

                                folderChildrenView =
                                    pgNodeView c

                                tailView =
                                    pgNodeView tail

                                currentFolderView =
                                    pgFolderView id name folderChildrenView open
                            in
                            currentFolderView :: tailView

                        File requestFileRecord ->
                            let
                                tailView =
                                    pgNodeView tail

                                currentFileView =
                                    Input.button []
                                        { onPress = Just (GenerateRandomUUIDForScene sceneParentId requestFileRecord.id PgActor)
                                        , label = el [] <| iconWithTextAndColor "label" (notEditedValue requestFileRecord.name) secondaryColor
                                        }
                            in
                            currentFileView :: tailView

        pgFolderView : Uuid -> Editable String -> List (Element Msg) -> Bool -> Element Msg
        pgFolderView id name folderChildrenView open =
            let
                folderIcon =
                    case open of
                        False ->
                            "keyboard_arrow_right"

                        True ->
                            "keyboard_arrow_down"
            in
            column []
                [ iconWithText folderIcon (notEditedValue name)
                , case open of
                    True ->
                        column [ spacing 10, paddingXY 20 10 ] folderChildrenView

                    False ->
                        none
                ]

    in
        column (box [ spacing 30, padding 30 ])
            [ el [ centerX ] <| text "Select a scene to add to your scenario"
            , treeView
            ]
