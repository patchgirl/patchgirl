module RequestBuilderApp.RequestBuilder.Run.App exposing (..)

import Animation
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
import RequestBuilderApp.RequestTree.Util as RequestTree
import RequestBuilderApp.RequestTree.App as RequestTree
import BuilderUtil exposing(..)
import Browser.Navigation as Navigation
import RequestBuilderApp.RequestBuilder.ResponseView exposing(..)
import Interpolator exposing(..)
import Application.Model as Application
import BuilderUtil exposing (..)
import List.Extra as List
import RequestComputation exposing (..)
import Dict
import Runner


-- * model


type alias Model a =
    { a
        | notification : Maybe Notification
        , requestCollection : RequestCollection
        , requestNewNode : NewNode
        , displayedRequestBuilderView : BuilderView Uuid
        , navigationKey : Navigation.Key
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , runnerRunning : Bool
        , page : Page
    }


-- * msg


type Msg
    = UpdateUrl String
    | SetHttpMethod HttpMethod
    | SetHttpBody String
    | SetHttpBodyResponse String
    | AddHeaderInput
    | UpdateHeaderKey Int String
    | UpdateHeaderValue Int String
    | DeleteHeader Int
    | AskRun
    | RemoteComputationDone RequestComputationOutput
    | PrintNotification Notification
    | AskSave
    | SaveSuccessfully
    | Animate Animation.Msg
    | ShowBodyResponseView
    | ShowHeaderResponseView


-- * update


update : Msg -> Model a -> RequestFileRecord -> (Model a, RequestFileRecord, Cmd Msg)
update msg model file =
    case msg of
        UpdateUrl newHttpUrl ->
            let
                newFile =
                    { file | httpUrl = changeEditedValue newHttpUrl file.httpUrl }
            in
            ( model, newFile, Cmd.none )

        SetHttpMethod newMethod ->
            let
                newFile =
                    { file | httpMethod = changeEditedValue newMethod file.httpMethod }
            in
            ( model, newFile, Cmd.none )

        UpdateHeaderKey idx newKey ->
            let
                editedHttpHeaders =
                    List.updateAt idx (\( _, value ) -> ( newKey, value )) (editedOrNotEditedValue file.httpHeaders)

                newHttpHeaders =
                    changeEditedValue editedHttpHeaders file.httpHeaders

                newFile =
                    { file | httpHeaders = newHttpHeaders }
            in
            ( model, newFile, Cmd.none )

        UpdateHeaderValue idx newValue ->
            let
                editedHttpHeaders =
                    List.updateAt idx (\( key, _ ) -> ( key, newValue )) (editedOrNotEditedValue file.httpHeaders)

                newHttpHeaders =
                    changeEditedValue editedHttpHeaders file.httpHeaders

                newFile =
                    { file | httpHeaders = newHttpHeaders }
            in
            ( model, newFile, Cmd.none )

        SetHttpBody httpBody ->
            let
                newFile =
                    { file | httpBody = changeEditedValue httpBody file.httpBody }
            in
            ( model, newFile, Cmd.none )

        AskSave ->
            let
                payload : Client.UpdateRequestFile
                payload =
                    { updateRequestFileName = editedOrNotEditedValue file.name
                    , updateRequestFileHttpUrl = editedOrNotEditedValue file.httpUrl
                    , updateRequestFileHttpMethod = Client.convertMethodFromFrontToBack (editedOrNotEditedValue file.httpMethod)
                    , updateRequestFileHttpHeaders = editedOrNotEditedValue file.httpHeaders
                    , updateRequestFileHttpBody = editedOrNotEditedValue file.httpBody
                    }

                (RequestCollection requestCollectionId _) =
                    model.requestCollection

                newMsg =
                    Client.putApiRequestCollectionByRequestCollectionIdByRequestNodeId "" "" requestCollectionId file.id payload updateRequestFileResultToMsg
            in
            ( model, file, newMsg )

        SaveSuccessfully ->
            let
                newFile =
                    { file
                        | name = NotEdited (editedOrNotEditedValue file.name)
                        , httpUrl = NotEdited (editedOrNotEditedValue file.httpUrl)
                        , httpMethod = NotEdited (editedOrNotEditedValue file.httpMethod)
                        , httpHeaders = NotEdited (editedOrNotEditedValue file.httpHeaders)
                        , httpBody = NotEdited (editedOrNotEditedValue file.httpBody)
                    }
            in
            ( model, newFile, Cmd.none )

        AskRun ->
            let
                keyValuesToRun : List (Storable NewKeyValue KeyValue)
                keyValuesToRun =
                    Application.getEnvironmentKeyValuesToRun model

                newMsg =
                    buildRequestToRun keyValuesToRun file model

                newRunRequestIconAnimation =
                    Animation.interrupt
                        [ Animation.loop
                            [ Animation.to [ Animation.rotate (Animation.turn 1) ]
                            , Animation.set [ Animation.rotate (Animation.turn 0) ]
                            ]
                        ]
                        file.runRequestIconAnimation

                newFile =
                    { file
                        | showResponseView = False
                        , requestComputationResult = Nothing
                        , runRequestIconAnimation = newRunRequestIconAnimation
                    }
            in
            ( model, newFile, newMsg )

        RemoteComputationDone remoteComputationResult ->
            let
                newRunRequestIconAnimation =
                    Animation.interrupt
                        [ Animation.to [ Animation.rotate (Animation.turn 1) ]
                        , Animation.set [ Animation.rotate (Animation.turn 0) ]
                        ]
                        file.runRequestIconAnimation

                newFile =
                    { file
                        | showResponseView = True
                        , requestComputationResult = Just remoteComputationResult
                        , runRequestIconAnimation = newRunRequestIconAnimation
                    }
            in
            ( model, newFile, Cmd.none )

        SetHttpBodyResponse newBody ->
            case file.requestComputationResult of
                Just (Ok response) ->
                    let
                        newRequestComputation =
                            { response | body = newBody }

                        newFile =
                            { file
                                | requestComputationResult = Just (Ok newRequestComputation)
                            }
                    in
                    ( model, newFile, Cmd.none )

                _ ->
                    ( model, file, Cmd.none )

        AddHeaderInput ->
            let
                newHttpHeaders =
                    Edited (notEditedValue file.httpHeaders) (editedOrNotEditedValue file.httpHeaders ++ [ ( "", "" ) ])

                newFile =
                    { file | httpHeaders = newHttpHeaders }
            in
            ( model, newFile, Cmd.none )

        DeleteHeader idx ->
            let
                editedHttpHeaders =
                    List.removeAt idx (editedOrNotEditedValue file.httpHeaders)

                newHttpHeaders =
                    changeEditedValue editedHttpHeaders file.httpHeaders

                newFile =
                    { file | httpHeaders = newHttpHeaders }
            in
            ( model, newFile, Cmd.none )

        Animate subMsg ->
            let
                newFile =
                    { file
                        | runRequestIconAnimation =
                            Animation.update subMsg file.runRequestIconAnimation
                    }
            in
            ( model, newFile, Cmd.none )

        ShowBodyResponseView ->
            let
                newFile =
                    { file | whichResponseView = BodyResponseView }
            in
            ( model, newFile, Cmd.none )

        ShowHeaderResponseView ->
            let
                newFile =
                    { file | whichResponseView = HeaderResponseView }
            in
            ( model, newFile, Cmd.none )

        PrintNotification notification ->
            let
                newRunRequestIconAnimation =
                    Animation.interrupt
                        [ Animation.to [ Animation.rotate (Animation.turn 1) ]
                        , Animation.set [ Animation.rotate (Animation.turn 0) ]
                        ]
                        file.runRequestIconAnimation

                newFile =
                    { file
                        | requestComputationResult = Nothing
                        , runRequestIconAnimation = newRunRequestIconAnimation
                    }

                newModel =
                    { model | notification = Just notification }
            in
            ( newModel, newFile, Cmd.none )


-- * util


isBuilderDirty : RequestFileRecord -> Bool
isBuilderDirty request =
    isDirty request.httpMethod
        || isDirty request.httpHeaders
        || List.any isDirty
            [ request.name
            , request.httpUrl
            , request.httpBody
            ]

buildRequestToRun : List (Storable NewKeyValue KeyValue) -> RequestFileRecord -> Model a -> Cmd Msg
buildRequestToRun envKeyValues file model =
    let
        request : RequestComputationInput
        request =
            buildRequestComputationInput file

        backRequestComputationInput =
            ( Client.convertRequestComputationInputFromFrontToBack request
            , envKeyValues
                |> List.map latestValueOfStorable
                |> List.map (\{ key, value } -> (key, value))
                |> List.map (Tuple.mapSecond Client.convertStringTemplateFromFrontToBack)
                |> Dict.fromList
            )
    in
    Client.postApiRunnerRequestComputation (Runner.runnerUrl model.runnerRunning) backRequestComputationInput remoteComputationDoneToMsg


-- ** message result


updateRequestFileResultToMsg : Result Http.Error () -> Msg
updateRequestFileResultToMsg result =
    case result of
        Ok () ->
            SaveSuccessfully

        Err error ->
            PrintNotification <| AlertNotification "" (httpErrorToString error)

remoteComputationDoneToMsg : Result Http.Error Client.RequestComputationOutput -> Msg
remoteComputationDoneToMsg result =
    case result of
        Ok backRequestComputationOutput ->
            RemoteComputationDone <|
                Client.convertRequestComputationOutputFromBackToFront backRequestComputationOutput

        Err error ->
            PrintNotification <| AlertNotification "Could not run HTTP request" (httpErrorToString error)

-- * view


view : RequestFileRecord -> Model a -> Element Msg
view file model =
    let
        keyValuesToRun : List (Storable NewKeyValue KeyValue)
        keyValuesToRun =
            Application.getEnvironmentKeyValuesToRun model

        builderView =
            column [ width fill, spacing 20 ]
                [ urlView model file keyValuesToRun
                , methodView model file
                , headersView model file keyValuesToRun
                , bodyView model file keyValuesToRun
                ]
    in
    case file.showResponseView of
        False ->
            column ( box [ width fill
                        , paddingXY 20 10
                        , spacing 10
                        , centerX
                        , padding 20
                        , width fill
                        ]
                   )
                [ el [ alignRight ] (closeBuilderView model.page)
                , titleView model file
                , el [ width fill ] builderView
                ]

        True ->
            wrappedRow [ alignTop, width fill, spacing 10 ]
                [ column
                    ( box [ width (fillPortion 1)
                         , alignTop
                         , spacing 10
                         , padding 20
                         ]
                    )
                    [ el [ alignRight ] (closeBuilderView model.page)
                    , titleView model file
                    , el [ alignTop ] builderView
                    ]
                , el
                    ( box [ width (fillPortion 1)
                         , alignTop
                         , padding 20
                         ]
                    )
                    (responseView model file)
                ]


-- ** title


titleView : Model a -> RequestFileRecord -> Element Msg
titleView model file =
    let
        name =
            notEditedValue file.name
    in
    row [ paddingXY 0 10, spacing 10, width fill ]
        [ el [] <| iconWithTextAndColor "label" name secondaryColor
        , el [ alignRight ] (mainActionButtonsView model file)
        ]


mainActionButtonsView : Model a -> RequestFileRecord -> Element Msg
mainActionButtonsView model file =
    let
        rowParam =
            [ centerY
            , height fill
            , spacing 10
            , alignRight
            , Font.color primaryColor
            ]

        inputParam =
            [ Border.solid
            , Border.color secondaryColor
            , Border.width 1
            , Border.rounded 5
            , Background.color secondaryColor
            , paddingXY 10 10
            ]

        runIcon : Element Msg
        runIcon =
            iconWithTextAndColorAndAttr "send" "Run" primaryColor []
    in
    row rowParam
        [ case isBuilderDirty file of
            True ->
                Input.button inputParam
                    { onPress = Just <| AskSave
                    , label = el [ centerY ] <| iconWithTextAndColor "save" "Save" primaryColor
                    }

            False ->
                none
        , Input.button inputParam
            { onPress = Just <| AskRun
            , label = el [ centerY ] runIcon
            }
        ]



-- ** response


responseView : Model a -> RequestFileRecord -> Element Msg
responseView model file =
    let
        errorAttributes =
            [ centerX
            , centerY
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
    in
    case file.requestComputationResult of
        Nothing ->
            none

        Just (Ok requestComputationOutput) ->
            column [ spacing 10, scrollbars, width fill ]
                [ statusResponseView requestComputationOutput
                , whichResponseButtonView
                      [ ("Body", file.whichResponseView == BodyResponseView, ShowBodyResponseView)
                      , ("Headers", file.whichResponseView == HeaderResponseView, ShowHeaderResponseView)
                      ]
                , case file.whichResponseView of
                      BodyResponseView ->
                          bodyResponseView requestComputationOutput SetHttpBodyResponse

                      HeaderResponseView ->
                          headersResponseView requestComputationOutput SetHttpBody
                ]

        Just (Err httpException) ->
            el errorAttributes (text (httpExceptionToString httpException))



-- ** url


urlView : Model a -> RequestFileRecord -> List (Storable NewKeyValue KeyValue) -> Element Msg
urlView model file keyValues =
    el [ alignLeft, width fill ] <|
        row [ width fill, spacing 30 ]
            [ Input.text [ Util.onEnter AskRun ]
                  { onChange = UpdateUrl
                  , text = editedOrNotEditedValue file.httpUrl
                  , placeholder = Just <| Input.placeholder [] (text "myApi.com/path?arg=someArg")
                  , label = Input.labelLeft [ centerY ] <| text "Url: "
                  }
            , showFullInterpolation "left-arrow-box" keyValues (editedOrNotEditedValue file.httpUrl)
            ]


-- ** method


methodView : Model a -> RequestFileRecord -> Element Msg
methodView model file =
    Input.radioRow [ padding 10, spacing 10 ]
        { onChange = SetHttpMethod
        , selected = Just <| editedOrNotEditedValue file.httpMethod
        , label = Input.labelLeft [ centerY ] <| text "Method: "
        , options =
            [ Input.option HttpGet (text "Get")
            , Input.option HttpPost (text "Post")
            , Input.option HttpPut (text "Put")
            , Input.option HttpDelete (text "Delete")
            , Input.option HttpPatch (text "Patch")
            , Input.option HttpHead (text "Head")
            , Input.option HttpOptions (text "Options")
            ]
        }



-- ** header


headersView : Model a -> RequestFileRecord -> List (Storable NewKeyValue KeyValue) -> Element Msg
headersView model file keyValues =
    let
        headerInputs =
            List.indexedMap (headerView model file keyValues) (editedOrNotEditedValue file.httpHeaders)

        addHeaderButton =
            Input.button [ centerX ]
                { onPress = Just <| AddHeaderInput
                , label =
                    row [ centerX ]
                        [ addIcon
                        , el [] (text "Add Header")
                        ]
                }
    in
        case List.isEmpty (editedOrNotEditedValue file.httpHeaders) of
            True ->
                el [ width fill, spacing 20 ] addHeaderButton

            False ->
                column [ width fill, spacing 20 ]
                    [ text "Headers:"
                    , column [ width fill, spacing 10 ] headerInputs
                    , addHeaderButton
                    ]


headerView : Model a -> RequestFileRecord -> List (Storable NewKeyValue KeyValue) -> Int -> ( String, String ) -> Element Msg
headerView model file keyValues idx ( headerKey, headerValue ) =
    row [ width fill, spacing 10 ]
        [ row [ width fill, spacing 30 ]
              [ Input.text [ Util.onEnter AskRun ]
                    { onChange = UpdateHeaderKey idx
                    , text = headerKey
                    , placeholder = Nothing
                    , label = Input.labelLeft [ centerY ] (text "key: ")
                    }
              , showFullInterpolation "left-arrow-box" keyValues headerKey
              ]
        , row [ width fill, spacing 30 ]
            [ Input.text [ Util.onEnter AskRun ]
                  { onChange = UpdateHeaderValue idx
                  , text = headerValue
                  , placeholder = Nothing
                  , label = Input.labelLeft [ centerY ] (text "value: ")
                  }
            , showFullInterpolation "left-arrow-box" keyValues headerValue
            ]
        , Input.button [ centerY ]
            { onPress = Just <| DeleteHeader idx
            , label =
                row [ centerX, centerY ]
                    [ deleteIcon
                    ]
            }
        ]



-- ** body


bodyView : Model a -> RequestFileRecord -> List (Storable NewKeyValue KeyValue) -> Element Msg
bodyView model file keyValues =
    wrappedRow [ width fill, spacing 30 ]
        [ Input.multiline []
              { onChange = SetHttpBody
              , text = editedOrNotEditedValue file.httpBody
              , placeholder = Just <| Input.placeholder [] (text "{}")
              , label = labelInputView "Body: "
              , spellcheck = False
              }
        , showOnlyInterpolation keyValues (editedOrNotEditedValue file.httpBody)
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


joinTuple : String -> ( String, String ) -> String
joinTuple separator ( key, value ) =
    key ++ separator ++ value
