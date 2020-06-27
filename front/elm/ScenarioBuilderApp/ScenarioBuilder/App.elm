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


-- * model


type alias Model =
    { session : Session
    , notification : Maybe String
    , whichModal : Maybe Modal
    , id : Uuid.Uuid
    , requestCollection : RequestCollection
    , scenarioCollectionId : Uuid
    , scenes : List Scene
    , keyValues : List (Storable NewKeyValue KeyValue)
    , name : Editable String
    , showDetailedSceneView : Maybe Uuid
    , whichResponseView : HttpResponseView
    , environments : List Environment
    , environmentId : Editable (Maybe Int)
    }


-- * message


type
    Msg
    -- create scene
    = ShowHttpRequestSelectionModal (Maybe Uuid)
    | GenerateRandomUUIDForScene (Maybe Uuid) Uuid
    | SelectRequestFile (Maybe Uuid) Uuid Uuid
    | AskCreateScene (Maybe Uuid) Uuid Uuid
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


        ShowHttpRequestSelectionModal sceneParentId ->
            let
                newModel =
                    { model | whichModal = Just (SelectHttpRequestModal sceneParentId) }
            in
            ( newModel, Cmd.none )

        GenerateRandomUUIDForScene sceneParentId requestFileNodeId ->
            let
                newMsg =
                    Random.generate (AskCreateScene sceneParentId requestFileNodeId) Uuid.uuidGenerator
            in
            ( model, newMsg )

        AskCreateScene sceneParentId requestFileNodeId newSceneId ->
            let
                payload =
                    { newSceneId = newSceneId
                    , newSceneSceneNodeParentId = sceneParentId
                    , newSceneRequestFileNodeId = requestFileNodeId
                    , newScenePrescript = ""
                    , newScenePostscript = ""
                    }

                newMsg =
                    Client.postApiScenarioNodeByScenarioNodeIdScene "" (getCsrfToken model.session) model.id payload (createSceneResultToMsg sceneParentId requestFileNodeId newSceneId)
            in
            ( model, newMsg )

        SelectRequestFile sceneParentId requestFileNodeId newSceneId ->
            let
                newScene =
                    mkDefaultScene newSceneId requestFileNodeId

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
                sceneToSceneInput : Scene -> Maybe Client.SceneInput
                sceneToSceneInput scene =
                    case (scene.prescriptAst, scene.postscriptAst) of
                        (Ok prescript, Ok postscript) ->
                            findFileRecord model scene.requestFileNodeId
                                |> Maybe.map buildRequestComputationInput
                                |> Maybe.map (\requestComputationInput ->
                                                  { sceneInputId = scene.id
                                                  , sceneInputRequestFileNodeId = scene.requestFileNodeId
                                                  , sceneInputTemplatedRequestComputationInput =
                                                      Client.convertRequestComputationInputFromFrontToBack requestComputationInput
                                                  , sceneInputPrescript =
                                                      Client.convertTangoscriptFromFrontToBack prescript
                                                  , sceneInputPostscript =
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

                mScenes : Maybe (List Client.SceneInput)
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
                            Client.postApiRunnerScenarioComputation "http://127.0.0.1:37465" payload runScenarioResultToMsg

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


        ShowDetailedView id ->
            let
                newModel =
                    { model | showDetailedSceneView = Just id }
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


mkDefaultScene : Uuid.Uuid -> Uuid.Uuid -> Scene
mkDefaultScene id requestFileNodeId =
    { id = id
    , requestFileNodeId = requestFileNodeId
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


createSceneResultToMsg : Maybe Uuid -> Uuid -> Uuid -> Result Http.Error () -> Msg
createSceneResultToMsg sceneParentId requestFileNodeId newSceneId result =
    case result of
        Ok () ->
            SelectRequestFile sceneParentId requestFileNodeId newSceneId

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
    in
    RequestTree.findFile requestNodes id


-- * view


view : Model -> Element Msg
view model =
    let
        scenesView =
            case model.scenes of
                [] ->
                    addNewSceneView

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
                [ row [ spacing 10]
                      [ Input.button [ centerX
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
                      , saveScenarioView
                      ]
                , envSelectionView
                ]

        sceneAndFileRecordDetailToShow : Maybe (Scene, RequestFileRecord)
        sceneAndFileRecordDetailToShow =
            let
                mScene =
                    Maybe.andThen (\sceneToShowId ->
                                   List.find (\scene -> scene.id == sceneToShowId) model.scenes) model.showDetailedSceneView

                mFileRecord =
                    Maybe.andThen (\scene -> findFileRecord model scene.requestFileNodeId) mScene

            in
            case (mScene, mFileRecord) of
                (Just scene, Just fileRecord) ->
                    Just (scene, fileRecord)

                _ ->
                    Nothing

    in
    case sceneAndFileRecordDetailToShow of
        Nothing ->
            wrappedRow [ height fill, width fill, spacing 20 ]
                [ el [ alignTop, Background.color white, boxShadow, padding 20 ] scenarioSettingView
                , el [] scenesView
                ]

        Just (scene, requestFileRecord) ->
            wrappedRow [ height fill, width fill, spacing 20 ]
                [ el [ width <| fillPortion 1, alignTop, Background.color white, boxShadow, padding 20 ] scenarioSettingView
                , row [ width <| fillPortion 9, spacing 20 ]
                    [ el [ width <| fillPortion 2, height fill ] scenesView
                    , el [ width <| fillPortion 8, height fill, alignRight ] (detailedSceneView model scene requestFileRecord)
                    ]
                ]



-- ** add new scene view


addNewSceneView : Element Msg
addNewSceneView =
    el [ width fill, centerX ]
        (Input.button [ centerX ]
            { onPress = Just (ShowHttpRequestSelectionModal Nothing)
            , label =
                row [ centerX, centerY ]
                    [ addIcon
                    , el [] (text "Add a scene to your scenario")
                    ]
            }
        )



-- ** scene view


sceneView : Model -> Scene -> Element Msg
sceneView model { id, requestFileNodeId, sceneComputation } =
    let
        (RequestCollection _ requestNodes) =
            model.requestCollection

        mRequestFileRecord =
            RequestTree.findFile requestNodes requestFileNodeId

        sceneComputationAttrs =
            case sceneComputation of
                Nothing ->
                    [ Border.color white ]

                Just (SceneSucceeded _) ->
                    [ borderSuccess, backgroundSuccess ]

                Just _ ->
                    [ borderError, backgroundError ]
    in
    case mRequestFileRecord of
        Just { name } ->
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
                    { onPress = Just (ShowDetailedView id)
                    , label =
                        row [ spacing 20, centerX ]
                            [ el [] (text (notEditedValue name))
                            , Input.button []
                                { onPress = Just (AskDeleteScene id)
                                , label = el [ alignRight ] clearIcon
                                }
                            ]
                    }
                , arrowView id
                ]

        _ ->
            none



-- ** arrow view


arrowView : Uuid.Uuid -> Element Msg
arrowView id =
    let
        addSceneBtn =
            Input.button []
                { onPress = Just (ShowHttpRequestSelectionModal (Just id))
                , label = row [] [ addIcon
                                 , text " add request"
                                 ]
                }
    in
        column [ centerX, spacing 5 ]
            [ el [ centerX ] addSceneBtn
            , el [ centerX ] arrowDownwardIcon
            ]



-- ** detailed scene view


detailedSceneView : Model -> Scene -> RequestFileRecord -> Element Msg
detailedSceneView model scene requestFileRecord =
    let
        { method, headers, url, body } =
            buildRequestComputationInput requestFileRecord

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
                          { url = href <| ReqPage (Just scene.requestFileNodeId) (Just model.id)
                          , label = el [] <| iconWithTextAndColor "label" (editedOrNotEditedValue requestFileRecord.name)
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

                RequestFailed httpException ->
                    text <| "This request failed because of: " ++ httpExceptionToString httpException

                PostscriptFailed scriptException ->
                    text <| "Postscript failed because of: " ++ scriptExceptionToString scriptException

                SceneSucceeded requestComputationOutput ->
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
    in
    column [ width fill
           , height fill
           , centerX
           , alignTop
           , spacing 40
           , padding 20
           , boxShadow
           , Background.color white
           ]
        <| case scene.sceneComputation of
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
    let
        size =
            width
                (fill
                    |> maximum 100
                    |> minimum 100
                )
    in
    Input.labelAbove [ centerY, size ] <| text labelText


-- * modal


selectHttpRequestModal : Maybe Uuid -> RequestCollection -> Modal.Config Msg
selectHttpRequestModal sceneParentId requestCollection =
    let
        (RequestCollection _ requestNodes) =
            requestCollection

        treeView =
            column [ spacing 10 ] (nodeView requestNodes)

        nodeView : List RequestNode -> List (Element Msg)
        nodeView nodes =
            case nodes of
                [] ->
                    []

                node :: tail ->
                    case node of
                        RequestFolder { id, name, open, children } ->
                            let
                                folderChildrenView =
                                    nodeView children

                                tailView =
                                    nodeView tail

                                currentFolderView =
                                    folderView id name folderChildrenView open
                            in
                            currentFolderView :: tailView

                        RequestFile requestFileRecord ->
                            let
                                tailView =
                                    nodeView tail

                                currentFileView =
                                    Input.button []
                                        { onPress = Just (GenerateRandomUUIDForScene sceneParentId requestFileRecord.id)
                                        , label = el [] <| iconWithTextAndColor "label" (notEditedValue requestFileRecord.name) secondaryColor
                                        }
                            in
                            currentFileView :: tailView

        folderView : Uuid.Uuid -> Editable String -> List (Element Msg) -> Bool -> Element Msg
        folderView id name folderChildrenView open =
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
    , header = text "Select an http request"
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
