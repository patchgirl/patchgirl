module ScenarioBuilderApp.ScenarioBuilder.App exposing (..)

import Api.Converter as Client
import Api.WebGeneratedClient as Client
import Api.RunnerGeneratedClient as Client
import TangoScript.Parser exposing (..)
import Dict
import Application.Type exposing (..)
import Json.Decode as Json
import Parser as P
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font
import Http
import List.Extra as List
import Modal exposing (Modal(..))
import Random
import RequestComputation exposing (..)
import Util exposing (..)
import Uuid exposing (Uuid)
import RequestComputation
import BuilderUtil exposing (..)
import RequestBuilderApp.RequestBuilder.ResponseView exposing(..)
import Page exposing(..)
import StringTemplate exposing (..)
import Dict exposing (Dict)
import Runner
import PGBuilderApp.PGBuilder.Run.App as PgBuilder
import Browser.Navigation as Navigation
import HttpError exposing(..)


-- * model


type alias Model =
    { session : Session
    , notification : Maybe Notification
    , whichModal : Maybe Modal
    , id : Uuid
    , requestCollection : RequestCollection
    , pgCollection : PgCollection
    , scenarioCollectionId : Uuid
    , scenes : List Scene
    , keyValues : List (Storable NewKeyValue KeyValue)
    , name : Editable String
    , displayedSceneId : Maybe Uuid
    , whichResponseView : HttpResponseView
    , environments : List Environment
    , environmentId : Editable (Maybe Uuid)
    , runnerRunning : Bool
    , navigationKey : Navigation.Key
    }


-- * message


type
    Msg
    -- create scene
    = ShowSceneSelectionModal (Maybe Uuid)
    | GenerateRandomUUIDForScene (Maybe Uuid) Uuid ActorType
    | SelectHttpFile (Maybe Uuid) Uuid Uuid
    | SelectPgFile (Maybe Uuid) Uuid Uuid
    | AskCreateScene (Maybe Uuid) Uuid ActorType Uuid
    | CloseModal
      -- delete scene
    | AskDeleteScene Uuid
    | DeleteScene Uuid
      -- update scene
    | UpdateScene Scene
    | SceneUpdated Scene
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of


-- ** create scene


        ShowSceneSelectionModal sceneParentId ->
            let
                newModel =
                    { model | whichModal = Just (SelectNewSceneModal sceneParentId) }
            in
            ( newModel, Cmd.none )

        GenerateRandomUUIDForScene sceneParentId fileNodeId actorType ->
            let
                newMsg =
                    Random.generate (AskCreateScene sceneParentId fileNodeId actorType) Uuid.uuidGenerator
            in
            ( model, newMsg )

        AskCreateScene sceneActorParentId fileNodeId actorType newSceneId ->
            let
                payload =
                    { newSceneId = newSceneId
                    , newSceneSceneActorParentId = sceneActorParentId
                    , newSceneActorId = fileNodeId
                    , newSceneActorType = Client.convertActorTypeFromFrontToBack actorType
                    , newScenePrescript = ""
                    , newScenePostscript = ""
                    }

                newMsg =
                    Client.postApiScenarioNodeByScenarioNodeIdScene "" (getCsrfToken model.session) model.id payload (createSceneResultToMsg sceneActorParentId fileNodeId newSceneId actorType)
            in
            ( model, newMsg )

        SelectHttpFile sceneParentId nodeId newSceneId ->
            let
                newScene =
                    mkDefaultScene newSceneId nodeId HttpActor

                newScenes =
                    case sceneParentId of
                        Nothing ->
                            model.scenes ++ [ newScene ]

                        Just parentId ->
                            addToListAfterPredicate model.scenes (\scene -> scene.id == parentId) newScene

                newModel =
                    { model
                        | whichModal = Nothing
                        , scenes = newScenes
                    }
            in
            ( newModel, Cmd.none )

        SelectPgFile sceneParentId nodeId newSceneId ->
            let
                newScene =
                    mkDefaultScene newSceneId nodeId PgActor

                newScenes =
                    case sceneParentId of
                        Nothing ->
                            model.scenes ++ [ newScene ]

                        Just parentId ->
                            addToListAfterPredicate model.scenes (\scene -> scene.id == parentId) newScene

                newModel =
                    { model
                        | whichModal = Nothing
                        , scenes = newScenes
                    }
            in
            ( newModel, Cmd.none )


-- ** delete scene


        AskDeleteScene sceneId ->
            let
                newMsg =
                    Client.deleteApiScenarioNodeByScenarioNodeIdSceneBySceneId "" (getCsrfToken model.session) model.id sceneId (deleteSceneResultToMsg sceneId)
            in
            ( model, newMsg )

        DeleteScene id ->
            let
                newScenes =
                    List.filter (not << (\scene -> scene.id == id)) model.scenes

                newModel =
                    { model | scenes = newScenes }

                newMsg =
                    case model.displayedSceneId == Just id of
                        True ->
                            Navigation.pushUrl model.navigationKey (href (ScenarioPage (Just model.id) Nothing))

                        False ->
                            Cmd.none

            in
            ( newModel, newMsg )


-- ** update scene


        UpdateScene scene ->
            let
                payload =
                    { updateScenePrescript = editedOrNotEditedValue scene.prescriptStr
                    , updateScenePostscript = editedOrNotEditedValue scene.postscriptStr
                    }

                newMsg =
                    Client.putApiScenarioNodeByScenarioNodeIdSceneBySceneId "" (getCsrfToken model.session) model.id scene.id payload (updateSceneResultToMsg scene)
            in
            ( model, newMsg )

        SceneUpdated updatedScene ->
            let
                newScene =
                    { updatedScene
                        | prescriptStr = NotEdited <| editedOrNotEditedValue updatedScene.prescriptStr
                        , postscriptStr = NotEdited <| editedOrNotEditedValue updatedScene.postscriptStr
                    }

                newScenes =
                    List.updateIf (\scene -> scene.id == updatedScene.id) (always newScene) model.scenes

                newModel =
                    { model | scenes = newScenes }
            in
            ( newModel, Cmd.none )


-- ** scenario


        AskSaveScenario newEnvironmentId ->
            let
                payload =
                    { updateScenarioFileId = model.id
                    , updateScenarioFileEnvironmentId = newEnvironmentId
                    }

                newMsg =
                    Client.putApiScenarioCollectionByScenarioCollectionIdScenarioFile "" (getCsrfToken model.session) model.scenarioCollectionId payload (updateScenarioResultToMsg newEnvironmentId)
            in
            (model, newMsg)

        UpdateScenarioFile newEnvironmentId ->
            let
                newModel =
                    { model | environmentId = NotEdited newEnvironmentId }
            in
            (newModel, Cmd.none)

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
                                    let
                                        (PgCollection collectionId _) =
                                            model.pgCollection
                                    in
                                        Debug.todo ""
                                            {-
                                    PgBuilderApp.convertFromFileToBuilder pgRecord collectionId model.keyValues Nothing
                                        |> PgBuilder.buildPgComputationPayload
                                        |> \(_, pgComputationInput) ->
                                           Just <| Client.PgSceneFile { sceneId = scene.id
                                                                      , sceneFileId = scene.nodeId
                                                                      , scenePrescript =
                                                                          Client.convertTangoscriptFromFrontToBack prescript
                                                                      , scenePostscript =
                                                                          Client.convertTangoscriptFromFrontToBack postscript
                                                                      , scenePgInput = pgComputationInput
                                                                      } -}

                        _ -> Nothing

                environmentKeyValues : Dict String (List Client.Template)
                environmentKeyValues =
                    editedOrNotEditedValue model.environmentId
                        |> Maybe.andThen (\scenarioEnvId -> List.find (\env -> (env.id == scenarioEnvId)) model.environments)
                        |> Maybe.map .keyValues
                        |> Maybe.withDefault []
                        |> List.map toLatestKeyValue
                        |> List.map (\{key, value} -> (key, Client.convertStringTemplateFromFrontToBack value))
                        |> Dict.fromList

                mScenes : Maybe (List Client.SceneFile)
                mScenes =
                    let
                        a =
                            model.scenes
                                |> List.map sceneToSceneInput
                    in
                        a |> traverseListMaybe

                mPayload =
                    mScenes
                        |> Maybe.map
                           (\scenes ->
                                { scenarioInputId = model.id
                                , scenarioInputScenes = scenes
                                , scenarioInputEnvVars = environmentKeyValues
                                }
                           )

                newMsg =
                    case mPayload of
                        Just payload ->
                            Client.postApiRunnerScenarioComputation Runner.desktopRunnerUrl payload runScenarioResultToMsg

                        Nothing ->
                            Cmd.none

                resetScenes =
                    model.scenes
                        |> List.map (\scene -> { scene | sceneComputation = Nothing })

                newModel =
                    { model | scenes = resetScenes }

            in
            ( newModel, newMsg )

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
                    List.map mergeSceneComputationOutputResult model.scenes

                newModel =
                    { model | scenes = newScenes }
            in
            ( newModel, Cmd.none )


-- ** detailed view

        ShowBodyResponseView ->
            let
                newModel =
                    { model | whichResponseView = BodyResponseView }
            in
            ( newModel, Cmd.none )

        ShowHeaderResponseView ->
            let
                newModel =
                    { model | whichResponseView = HeaderResponseView }
            in
            ( newModel, Cmd.none )


-- ** script


        SetPrescript scene newScriptStr ->
            let
                newScene =
                    { scene
                        | prescriptStr = changeEditedValue newScriptStr scene.prescriptStr
                        , prescriptAst = parseTangoscript newScriptStr
                    }

                newScenes =
                    List.updateIf (\s -> s.id == scene.id) (always newScene) model.scenes

                newModel =
                    { model | scenes = newScenes }
            in
            ( newModel, Cmd.none )

        SetPostscript scene newScriptStr ->
            let
                newScene =
                    { scene
                        | postscriptStr = changeEditedValue newScriptStr scene.postscriptStr
                        , postscriptAst = parseTangoscript newScriptStr
                    }

                newScenes =
                    List.updateIf (\s -> s.id == scene.id) (always newScene) model.scenes

                newModel =
                    { model | scenes = newScenes }
            in
            ( newModel, Cmd.none )


-- ** other


        SetEnvironmentId mEnvId ->
            let
                newModel =
                    { model | environmentId = changeEditedValue mEnvId model.environmentId }
            in
            ( newModel, Cmd.none )


        CloseModal ->
            let
                newModel =
                    { model | whichModal = Nothing }
            in
            ( newModel, Cmd.none )

        PrintNotification notification ->
            ( { model | notification = Just notification }, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )


-- * util


mkDefaultScene : Uuid -> Uuid -> ActorType -> Scene
mkDefaultScene id nodeId actorType =
    { id = id
    , nodeId = nodeId
    , actorType = actorType
    , sceneComputation = Nothing
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
                AlertNotification ("Could not run SQL request. Is <a href=\"" ++ (href (DocumentationPage PatchGirlRunnerAppDoc)) ++ "\">patchgirl-runner</a> running?") (httpErrorToString err)


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
                    SelectHttpFile sceneParentId nodeId newSceneId

                PgActor ->
                    SelectPgFile sceneParentId nodeId newSceneId

        Err err ->
            PrintNotification <| AlertNotification "Could not create the scene, try reloading the page!" (httpErrorToString err)

updateSceneResultToMsg : Scene -> Result Http.Error () -> Msg
updateSceneResultToMsg scene result =
    case result of
        Ok () ->
            SceneUpdated scene

        Err err ->
            PrintNotification <| AlertNotification "Could not update the scene, try reloading the page!" (httpErrorToString err)

updateScenarioResultToMsg : Maybe Uuid -> Result Http.Error () -> Msg
updateScenarioResultToMsg newEnvironmentId result =
    case result of
        Ok () ->
            UpdateScenarioFile newEnvironmentId

        Err err ->
            PrintNotification  <| AlertNotification "Could not update the scenario, try reloading the page!" (httpErrorToString err)

findRecord : Model -> Scene -> Maybe FileRecord
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


-- * view


view : Model -> Element Msg
view model =
    let
        scenesView =
            case model.scenes of
                [] ->
                    Input.button [ centerY, centerX ]
                        { onPress = Just (ShowSceneSelectionModal Nothing)
                        , label =
                            row [ centerX, centerY ]
                                [ addIcon
                                , el [] (text "Add a scene to your scenario")
                                ]
                        }

                scenes ->
                    column [ centerX, spacing 10 ] (List.map (sceneView model) scenes)

        envSelectionView : Element Msg
        envSelectionView =
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
            Input.radio [ padding 10, spacing 10 ]
                { onChange = SetEnvironmentId
                , selected = Just (editedOrNotEditedValue model.environmentId)
                , label = Input.labelAbove [] <| text "Environment to run with: "
                , options = noEnvironmentOption :: List.map option model.environments
                }

        saveScenarioView : Element Msg
        saveScenarioView =
            case model.environmentId of
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

        scenarioSettingView : Element Msg
        scenarioSettingView =
            column [ width fill, centerX, spacing 20 ]
                [ row [ spacing 10, width fill ]
                      [ el [] <| iconWithTextAndColor "label" (editedOrNotEditedValue model.name) secondaryColor
                      , el [ alignRight ] saveScenarioView
                      , el [ alignRight ] <|
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
                                  row [ centerX, centerY ]
                                      [ iconWithTextAndColorAndAttr "send" "Run" primaryColor []
                                      ]
                              }
                      ]
                , envSelectionView
                ]
    in
    case model.displayedSceneId of
        Nothing ->
            wrappedRow [ height fill, width fill, spacing 20 ]
                [ el ( box [ width <| fillPortion 1, alignTop, padding 20 ] ) scenarioSettingView
                , row [ width <| fillPortion 9, alignTop, spacing 20 ]
                    [ el [ width <| fillPortion 2, height fill ] scenesView
                    , el [ width <| fillPortion 8, height fill, alignRight ] none
                    ]
                ]

        Just sceneId ->
            wrappedRow [ height fill, width fill, spacing 20 ]
                [ el ( box [ width <| fillPortion 1, alignTop, padding 20 ] ) scenarioSettingView
                , row [ width <| fillPortion 9, alignTop, spacing 20 ]
                    [ el [ width <| fillPortion 2, height fill ] scenesView
                    , el [ width <| fillPortion 8, height fill, alignRight ] (detailedSceneView model sceneId)
                    ]
                ]


-- ** scene view


sceneView : Model -> Scene -> Element Msg
sceneView model scene =
    let
        selected =
            model.displayedSceneId == Just scene.id

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
                { id, name } =
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
                    ( box [ Border.width 1, centerX ] ++ sceneComputationAttrs ++ [ selectedSceneAttrs ]
                    ) [ link [ padding 20, width fill, height fill ]
                           { url = href <| ScenarioPage (Just model.id) (Just scene.id)
                           , label =
                               row [ spacing 20, centerX ]
                                   [ row []
                                         [ iconWithAttr { defaultIconAttribute | icon = sceneIcon, title = "" }
                                         , text (notEditedValue name)
                                         ]
                                   ]
                           }
                     , Input.button []
                            { onPress = Just (AskDeleteScene scene.id)
                            , label = el [ alignRight ] clearIcon
                            }
                      , el [ width (px 10) ] none
                      ]
                , arrowView scene.id
                ]

        _ ->
            none



-- ** arrow view


arrowView : Uuid -> Element Msg
arrowView id =
    let
        addSceneBtn =
            Input.button []
                { onPress = Just (ShowSceneSelectionModal (Just id))
                , label = row [] [ addIcon
                                 , text " add new scene"
                                 ]
                }
    in
        column [ centerX, spacing 5 ]
            [ el [ centerX ] addSceneBtn
            , el [ centerX ] arrowDownwardIcon
            ]



-- ** detailed scene view


detailedSceneView : Model -> Uuid -> Element Msg
detailedSceneView model sceneId =
    let
        mSceneAndRecord =
            List.find (\scene -> scene.id == sceneId) (model.scenes)
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
                        httpDetailedSceneView model scene record

                    PgRecord record ->
                        pgDetailedSceneView model scene record


-- *** http detailed scene view


httpDetailedSceneView : Model -> Scene -> RequestFileRecord -> List (Element Msg)
httpDetailedSceneView model scene fileRecord =
    let
        { method, headers, url, body } =
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
                        { onPress = Just (UpdateScene scene)
                        , label =
                            row [ centerX, centerY ]
                                [ iconWithTextAndColorAndAttr "save" "Save" primaryColor []
                                ]
                        }

        sceneInputDetailView =
            column [ width fill, spacing 15 ]
              [ row [ spacing 10, width fill ]
                    [ link [ alignLeft ]
                          { url = href <| ReqPage (RunView model.id)
                          , label = el [ Font.underline, Font.size 25 ] <|
                                    iconWithAttr { defaultIconAttribute
                                                     | icon = "public"
                                                     , title = (editedOrNotEditedValue fileRecord.name)
                                                     , primIconColor = Just primaryColor
                                                 }
                          }
                    , saveSceneButton
                    , link [ alignRight ]
                        { url = href <| ScenarioPage (Just model.id) Nothing
                        , label = el [ alignRight ] clearIcon
                        }
                    ]
              , Input.multiline [ Background.color lightGrey ]
                  { onChange = always DoNothing
                  , text = methodAndUrl
                  , placeholder = Nothing
                  , label = Input.labelHidden "http gist"
                  , spellcheck = False
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
                    text <| "This request failed because of: " ++ httpExceptionToString httpException

                PgSceneFailed error ->
                    none

                HttpPostscriptFailed _ scriptException ->
                    text <| "Postscript failed because of: " ++ scriptExceptionToString scriptException

                PgPostscriptFailed _ _ ->
                    none

                HttpSceneOk requestComputationOutput ->
                    column [ width fill ]
                        [ statusResponseView requestComputationOutput
                        , whichResponseButtonView
                              [ ("Body", model.whichResponseView == BodyResponseView, ShowBodyResponseView)
                              , ("Headers", model.whichResponseView == HeaderResponseView, ShowHeaderResponseView)
                              ]
                        , case model.whichResponseView of
                              BodyResponseView ->
                                  bodyResponseView requestComputationOutput (always DoNothing)

                              HeaderResponseView ->
                                  headersResponseView requestComputationOutput (always DoNothing)
                        ]

                PgSceneOk pgComputation ->
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


pgDetailedSceneView : Model -> Scene -> PgFileRecord -> List (Element Msg)
pgDetailedSceneView model scene fileRecord =
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
                        { onPress = Just (UpdateScene scene)
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
                        { url =  href <| ScenarioPage (Just model.id) (Nothing)
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

                HttpSceneOk requestComputationOutput ->
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


-- * modal


selectSceneModal : Maybe Uuid -> RequestCollection -> PgCollection -> Modal.Config Msg
selectSceneModal sceneParentId requestCollection pgCollection =
    let
        (RequestCollection _ requestNodes) =
            requestCollection

        (PgCollection _ pgNodes) =
            pgCollection

        treeView =
            row [ width fill ]
                [ column [ spacing 30, width (fillPortion 1) ]
                      [ el [ centerX, Font.size 23 ] (text "Http request")
                      , column [ spacing 10] (httpNodeView requestNodes)
                      ]
                , column [ spacing 30, width (fillPortion 1), alignTop ]
                      [ el [ centerX, Font.size 23 ] (text "Postgres query")
                      , column [ spacing 10] (pgNodeView pgNodes)
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
    { closeMessage = CloseModal
    , header = text "Add either an http request or a postgres query"
    , body = Just treeView
    , footer = Nothing
    }

confirmDeleteFolderModal : Modal.Config Msg
confirmDeleteFolderModal =
    { closeMessage = CloseModal
    , header = text "confirm delete folder"
    , body = Nothing
    , footer = Nothing
    }
