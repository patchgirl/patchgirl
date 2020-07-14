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
import RequestBuilderApp.RequestTree.Util as RequestTree
import RequestComputation exposing (..)
import Util exposing (..)
import Uuid exposing (Uuid)
import RequestComputation
import RequestBuilderApp.RequestBuilder.ResponseView exposing(..)
import Page exposing(..)
import StringTemplate exposing (..)
import Dict exposing (Dict)
import Runner
import PGBuilderApp.PGTree.Util as PgTree


-- * model


type alias Model =
    { session : Session
    , notification : Maybe String
    , whichModal : Maybe Modal
    , id : Uuid.Uuid
    , requestCollection : RequestCollection
    , pgCollection : PgCollection
    , scenarioCollectionId : Uuid
    , scenes : List Scene
    , keyValues : List (Storable NewKeyValue KeyValue)
    , name : Editable String
    , showDetailedSceneView : Maybe Uuid
    , whichResponseView : HttpResponseView
    , environments : List Environment
    , environmentId : Editable (Maybe Int)
    , runnerRunning : Bool
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
    | ServerError
      -- update scene
    | UpdateScene Scene
    | SceneUpdated Scene
      -- scenario
    | AskSaveScenario (Maybe Int)
    | UpdateScenarioFile (Maybe Int)
    | AskRunScenario
    | ScenarioProcessed ScenarioOutput
      -- detailed view
    | ShowDetailedView Uuid
    | HideDetailedView
    | ShowBodyResponseView
    | ShowHeaderResponseView
    -- script
    | SetPrescript Scene String
    | SetPostscript Scene String
      -- other
    | SetEnvironmentId (Maybe Int)
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

        AskCreateScene sceneParentId fileNodeId actorType newSceneId ->
            let
                payload =
                    { newSceneId = newSceneId
                    , newSceneSceneNodeParentId = sceneParentId
                    , newSceneActorId = fileNodeId
                    , newSceneActorType = Client.convertActorTypeFromFrontToBack actorType
                    , newScenePrescript = ""
                    , newScenePostscript = ""
                    }

                newMsg =
                    Client.postApiScenarioNodeByScenarioNodeIdScene "" (getCsrfToken model.session) model.id payload (createSceneResultToMsg sceneParentId fileNodeId newSceneId actorType)
            in
            ( model, newMsg )

        SelectHttpFile sceneParentId nodeId newSceneId ->
            let
                newScene =
                    mkDefaultScene newSceneId nodeId HttpScene

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
                    mkDefaultScene newSceneId nodeId PgScene

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
            in
            ( newModel, Cmd.none )


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
                    case (scene.prescriptAst, scene.postscriptAst) of
                        (Ok prescript, Ok postscript) ->
                            findFileRecord model scene.nodeId
                                |> Maybe.map buildRequestComputationInput
                                |> Maybe.map (\requestComputationInput ->
                                                  Client.HttpSceneFile { sceneId = scene.id
                                                                       , sceneFileId = scene.nodeId
                                                                       , sceneHttpInput =
                                                                             (Client.convertRequestComputationInputFromFrontToBack requestComputationInput)
                                                                       , scenePrescript =
                                                                           Client.convertTangoscriptFromFrontToBack prescript
                                                                       , scenePostscript =
                                                                           Client.convertTangoscriptFromFrontToBack postscript
                                                                       })

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
                    model.scenes
                        |> List.map sceneToSceneInput
                        |> traverseListMaybe

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
                            Client.postApiRunnerScenarioComputation (Runner.runnerUrl model.runnerRunning) payload runScenarioResultToMsg

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


        ShowDetailedView sceneId ->
            let
                newModel =
                    { model | showDetailedSceneView = Just sceneId }
            in
            (newModel, Cmd.none)

        HideDetailedView ->
            let
                newModel =
                    { model | showDetailedSceneView = Nothing }
            in
            (newModel, Cmd.none)

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

        ServerError ->
            ( model, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )


-- * util


mkDefaultScene : Uuid.Uuid -> Uuid.Uuid -> ActorType -> Scene
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

        Err error ->
            Debug.todo "server error" ServerError


deleteSceneResultToMsg : Uuid.Uuid -> Result Http.Error () -> Msg
deleteSceneResultToMsg sceneId result =
    case result of
        Ok () ->
            DeleteScene sceneId

        Err error ->
            Debug.todo "server error" ServerError


createSceneResultToMsg : Maybe Uuid -> Uuid -> Uuid -> ActorType -> Result Http.Error () -> Msg
createSceneResultToMsg sceneParentId nodeId newSceneId actorType result =
    case result of
        Ok () ->
            case actorType of
                HttpScene ->
                    SelectHttpFile sceneParentId nodeId newSceneId

                PgScene ->
                    SelectPgFile sceneParentId nodeId newSceneId

        Err error ->
            Debug.todo "server error" ServerError

updateSceneResultToMsg : Scene -> Result Http.Error () -> Msg
updateSceneResultToMsg scene result =
    case result of
        Ok () ->
            SceneUpdated scene

        Err error ->
            Debug.todo "server error" ServerError

updateScenarioResultToMsg : Maybe Int -> Result Http.Error () -> Msg
updateScenarioResultToMsg newEnvironmentId result =
    case result of
        Ok () ->
            UpdateScenarioFile newEnvironmentId

        Err error ->
            Debug.todo "server error" ServerError

findFileRecord : Model -> Uuid -> Maybe RequestFileRecord
findFileRecord model id =
    let
        (RequestCollection _ requestNodes) =
            model.requestCollection

        (PgCollection _ pgNodes) =
            model.pgCollection
    in
    RequestTree.findFile requestNodes id


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
                noEnvironmentOption : Input.Option (Maybe Int) Msg
                noEnvironmentOption =
                    Input.option Nothing <|
                        el [ width fill ] (text "No environment")

                option : Environment -> Input.Option (Maybe Int) Msg
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
    case model.showDetailedSceneView of
        Nothing ->
            wrappedRow [ height fill, width fill, spacing 20 ]
                [ el [ alignTop, Background.color white, boxShadow, padding 20 ] scenarioSettingView
                , el [ alignTop, height fill ] scenesView
                ]

        Just sceneId ->
            wrappedRow [ height fill, width fill, spacing 20 ]
                [ el [ width <| fillPortion 1, alignTop, Background.color white, boxShadow, padding 20 ] scenarioSettingView
                , row [ width <| fillPortion 9, alignTop, spacing 20 ]
                    [ el [ width <| fillPortion 2, height fill ] scenesView
                    , el [ width <| fillPortion 8, height fill, alignRight ] (detailedSceneView model sceneId)
                    ]
                ]


-- ** scene view


sceneView : Model -> Scene -> Element Msg
sceneView model scene =
    let
        (RequestCollection _ requestNodes) = model.requestCollection
        (PgCollection _ pgNodes) = model.pgCollection

        mFileRecord : Maybe FileRecord
        mFileRecord =
            case scene.actorType of
                HttpScene ->
                    RequestTree.findFile requestNodes scene.nodeId |> Maybe.map HttpRecord

                PgScene ->
                    PgTree.findFile pgNodes scene.nodeId |> Maybe.map PgRecord

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
    case mFileRecord of
        Just fileRecord ->
            let
                { id, name } =
                    case fileRecord of
                        HttpRecord r -> { id = r.id, name = r.name }
                        PgRecord r -> { id = r.id, name = r.name }
            in
            column [ centerX, spacing 10 ]
                [ Input.button
                    ( [ Border.solid
                      , Border.width 1
                      , Border.rounded 5
                      , Background.color white
                      , padding 20
                      , boxShadow
                      , centerX
                      ] ++ sceneComputationAttrs
                    )
                    { onPress = Just (ShowDetailedView scene.id)
                    , label =
                        row [ spacing 20, centerX ]
                            [ el [] (text (notEditedValue name))
                            , Input.button []
                                { onPress = Just (AskDeleteScene id)
                                , label = el [ alignRight ] clearIcon
                                }
                            ]
                    }
                , arrowView scene.id
                ]

        _ ->
            none



-- ** arrow view


arrowView : Uuid.Uuid -> Element Msg
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
        findRecord : Scene -> Maybe FileRecord
        findRecord scene =
            let
                (RequestCollection _ requestNodes) =
                    model.requestCollection

                (PgCollection _ pgNodes) =
                    model.pgCollection
            in
            case scene.actorType of
                HttpScene ->
                    RequestTree.findFile requestNodes scene.nodeId |> Maybe.map HttpRecord

                PgScene ->
                    PgTree.findFile pgNodes scene.nodeId |> Maybe.map PgRecord

        mSceneAndRecord =
            List.find (\scene -> scene.id == sceneId) model.scenes
                |> Maybe.andThen (\scene -> (findRecord scene) |> Maybe.map (\record -> (scene, record)))
    in
    case mSceneAndRecord of
        Nothing ->
            none

        Just (scene, fileRecord) ->
            column [ width fill
               , height fill
               , centerX
               , alignTop
               , spacing 40
               , padding 20
               , boxShadow
               , Background.color white
               ] <|
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
                    Input.button [ centerX
                                 , Border.solid
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
            column [ spacing 10 ]
              [ row [ spacing 10 ]
                    [ link []
                          { url = href <| ReqPage (Just scene.nodeId) (Just model.id)
                          , label = el [ Font.underline ] <| iconWithTextAndColor "label" (editedOrNotEditedValue fileRecord.name)
                                    secondaryColor
                          }
                    , saveSceneButton
                    ]
              , el [ ] (text methodAndUrl)
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
                    text <| "This postgresql query failed because of: " ++ error

                HttpPostscriptFailed _ scriptException ->
                    text <| "Postscript failed because of: " ++ scriptExceptionToString scriptException

                PgPostscriptFailed _ scriptException ->
                    text <| "Postscript failed because of: " ++ scriptExceptionToString scriptException

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
                    column [ width fill ]
                        []


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
        sceneInputDetailView =
            column [ spacing 10 ]
              [ row [ spacing 10 ]
                    [ link []
                          { url = href <| PgPage (Just fileRecord.id)
                          , label = el [ Font.underline ] <| iconWithTextAndColor "label" (editedOrNotEditedValue fileRecord.name)
                                    secondaryColor
                          }
                    ]
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
                    text <| "This postgresql query failed because of: " ++ error

                HttpPostscriptFailed _ scriptException ->
                    text <| "Postscript failed because of: " ++ scriptExceptionToString scriptException

                PgPostscriptFailed _ scriptException ->
                    text <| "Postscript failed because of: " ++ scriptExceptionToString scriptException

                HttpSceneOk requestComputationOutput ->
                    none

                PgSceneOk pgComputation ->
                    column [ width fill ]
                        []


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
                        RequestFolder { id, name, open, children } ->
                            let
                                folderChildrenView =
                                    httpNodeView children

                                tailView =
                                    httpNodeView tail

                                currentFolderView =
                                    httpFolderView id name folderChildrenView open
                            in
                            currentFolderView :: tailView

                        RequestFile requestFileRecord ->
                            let
                                tailView =
                                    httpNodeView tail

                                currentFileView =
                                    Input.button []
                                        { onPress = Just (GenerateRandomUUIDForScene sceneParentId requestFileRecord.id HttpScene)
                                        , label = el [] <| iconWithTextAndColor "label" (notEditedValue requestFileRecord.name) secondaryColor
                                        }
                            in
                            currentFileView :: tailView

        httpFolderView : Uuid.Uuid -> Editable String -> List (Element Msg) -> Bool -> Element Msg
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
                        PgFolder { id, name, open, children } ->
                            let
                                folderChildrenView =
                                    pgNodeView children

                                tailView =
                                    pgNodeView tail

                                currentFolderView =
                                    pgFolderView id name folderChildrenView open
                            in
                            currentFolderView :: tailView

                        PgFile requestFileRecord ->
                            let
                                tailView =
                                    pgNodeView tail

                                currentFileView =
                                    Input.button []
                                        { onPress = Just (GenerateRandomUUIDForScene sceneParentId requestFileRecord.id PgScene)
                                        , label = el [] <| iconWithTextAndColor "label" (notEditedValue requestFileRecord.name) secondaryColor
                                        }
                            in
                            currentFileView :: tailView

        pgFolderView : Uuid.Uuid -> Editable String -> List (Element Msg) -> Bool -> Element Msg
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
