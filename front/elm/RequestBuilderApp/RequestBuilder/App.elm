module RequestBuilderApp.RequestBuilder.App exposing (..)

import Animation
import Api.Converter as Client
import Api.WebGeneratedClient as Client
import Api.RunnerGeneratedClient as Client
import Application.Type exposing (..)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html as Html
import Html.Attributes as Html
import Html.Events as Html
import Http
import Json.Decode as Json
import Json.Print as Json
import List.Extra as List
import PrivateAddress exposing (..)
import RequestComputation exposing (..)
import Util exposing (..)
import Uuid exposing (Uuid)
import RequestBuilderApp.RequestBuilder.ResponseView exposing(..)
import Page exposing(..)
import Runner


-- * model


type alias Model =
    { notification : Maybe Notification
    , id : Uuid
    , requestCollectionId : Int
    , keyValues : List (Storable NewKeyValue KeyValue)
    , name : Editable String
    , httpUrl : Editable String
    , httpMethod : Editable HttpMethod
    , httpHeaders : Editable (List ( String, String ))
    , httpBody : Editable String
    , requestComputationResult : Maybe RequestComputationOutput
    , showResponseView : Bool
    , whichResponseView : HttpResponseView
    , runRequestIconAnimation : Animation.State
    , runnerRunning : Bool
    }



-- * message


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
    | LocalComputationDone (Result DetailedError ( Http.Metadata, String )) -- request ran from the browser
    | RemoteComputationDone RequestComputationOutput -- request ran from the server
    | RemoteComputationFailed
    | ServerError
    | AskSave
    | SaveSuccessfully
    | Animate Animation.Msg
    | ShowBodyResponseView
    | ShowHeaderResponseView


-- * update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateUrl newHttpUrl ->
            let
                newModel =
                    { model | httpUrl = changeEditedValue newHttpUrl model.httpUrl }
            in
            ( newModel, Cmd.none )

        SetHttpMethod newMethod ->
            let
                newModel =
                    { model | httpMethod = changeEditedValue newMethod model.httpMethod }
            in
            ( newModel, Cmd.none )

        UpdateHeaderKey idx newKey ->
            let
                editedHttpHeaders =
                    List.updateAt idx (\( _, value ) -> ( newKey, value )) (editedOrNotEditedValue model.httpHeaders)

                newHttpHeaders =
                    changeEditedValue editedHttpHeaders model.httpHeaders

                newModel =
                    { model | httpHeaders = newHttpHeaders }
            in
            ( newModel, Cmd.none )

        UpdateHeaderValue idx newValue ->
            let
                editedHttpHeaders =
                    List.updateAt idx (\( key, _ ) -> ( key, newValue )) (editedOrNotEditedValue model.httpHeaders)

                newHttpHeaders =
                    changeEditedValue editedHttpHeaders model.httpHeaders

                newModel =
                    { model | httpHeaders = newHttpHeaders }
            in
            ( newModel, Cmd.none )

        SetHttpBody httpBody ->
            let
                newModel =
                    { model | httpBody = changeEditedValue httpBody model.httpBody }
            in
            ( newModel, Cmd.none )

        AskSave ->
            let
                payload : Client.UpdateRequestFile
                payload =
                    { updateRequestFileName = editedOrNotEditedValue model.name
                    , updateRequestFileHttpUrl = editedOrNotEditedValue model.httpUrl
                    , updateRequestFileHttpMethod = Client.convertMethodFromFrontToBack (editedOrNotEditedValue model.httpMethod)
                    , updateRequestFileHttpHeaders = editedOrNotEditedValue model.httpHeaders
                    , updateRequestFileHttpBody = editedOrNotEditedValue model.httpBody
                    }

                newMsg =
                    Client.putApiRequestCollectionByRequestCollectionIdByRequestNodeId "" "" model.requestCollectionId model.id payload updateRequestFileResultToMsg
            in
            ( model, newMsg )

        SaveSuccessfully ->
            let
                newModel =
                    { model
                        | name = NotEdited (editedOrNotEditedValue model.name)
                        , httpUrl = NotEdited (editedOrNotEditedValue model.httpUrl)
                        , httpMethod = NotEdited (editedOrNotEditedValue model.httpMethod)
                        , httpHeaders = NotEdited (editedOrNotEditedValue model.httpHeaders)
                        , httpBody = NotEdited (editedOrNotEditedValue model.httpBody)
                    }
            in
            ( newModel, Cmd.none )

        AskRun ->
            let
                newMsg =
                    buildRequestToRun model.keyValues model

                newRunRequestIconAnimation =
                    Animation.interrupt
                        [ Animation.loop
                            [ Animation.to [ Animation.rotate (Animation.turn 1) ]
                            , Animation.set [ Animation.rotate (Animation.turn 0) ]
                            ]
                        ]
                        model.runRequestIconAnimation

                newModel =
                    { model
                        | showResponseView = False
                        , requestComputationResult = Nothing
                        , runRequestIconAnimation = newRunRequestIconAnimation
                    }
            in
            ( newModel, newMsg )

        RemoteComputationDone remoteComputationResult ->
            let
                newRunRequestIconAnimation =
                    Animation.interrupt
                        [ Animation.to [ Animation.rotate (Animation.turn 1) ]
                        , Animation.set [ Animation.rotate (Animation.turn 0) ]
                        ]
                        model.runRequestIconAnimation

                newModel =
                    { model
                        | showResponseView = True
                        , requestComputationResult = Just remoteComputationResult
                        , runRequestIconAnimation = newRunRequestIconAnimation
                    }
            in
            ( newModel, Cmd.none )

        RemoteComputationFailed ->
            let
                newRunRequestIconAnimation =
                    Animation.interrupt
                        [ Animation.to [ Animation.rotate (Animation.turn 1) ]
                        , Animation.set [ Animation.rotate (Animation.turn 0) ]
                        ]
                        model.runRequestIconAnimation

                newModel =
                    { model
                        | requestComputationResult = Nothing
                        , runRequestIconAnimation = newRunRequestIconAnimation
                    }
            in
            ( newModel, Cmd.none )

        LocalComputationDone result ->
            let
                newModel =
                    { model
                        | requestComputationResult = Just (convertResultToResponse result)
                    }
            in
            ( newModel, Cmd.none )

        SetHttpBodyResponse newBody ->
            case model.requestComputationResult of
                Just (Ok response) ->
                    let
                        newRequestComputation =
                            { response | body = newBody }

                        newModel =
                            { model
                                | requestComputationResult = Just (Ok newRequestComputation)
                            }
                    in
                    ( newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AddHeaderInput ->
            let
                newHttpHeaders =
                    Edited (notEditedValue model.httpHeaders) (editedOrNotEditedValue model.httpHeaders ++ [ ( "", "" ) ])

                newModel =
                    { model | httpHeaders = newHttpHeaders }
            in
            ( newModel, Cmd.none )

        DeleteHeader idx ->
            let
                editedHttpHeaders =
                    List.removeAt idx (editedOrNotEditedValue model.httpHeaders)

                newHttpHeaders =
                    changeEditedValue editedHttpHeaders model.httpHeaders

                newModel =
                    { model | httpHeaders = newHttpHeaders }
            in
            ( newModel, Cmd.none )

        Animate subMsg ->
            let
                newModel =
                    { model
                        | runRequestIconAnimation =
                            Animation.update subMsg model.runRequestIconAnimation
                    }
            in
            ( newModel, Cmd.none )

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

        ServerError ->
            let
                newModel =
                    { model | notification = Just (AlertNotification "server error") }
            in
            ( newModel, Cmd.none )



-- * util


isBuilderDirty : Model -> Bool
isBuilderDirty model =
    isDirty model.httpMethod
        || isDirty model.httpHeaders
        || List.any isDirty
            [ model.name
            , model.httpUrl
            , model.httpBody
            ]


updateRequestFileResultToMsg : Result Http.Error () -> Msg
updateRequestFileResultToMsg result =
    case result of
        Ok () ->
            SaveSuccessfully

        Err error ->
            ServerError


remoteComputationDoneToMsg : Result Http.Error Client.RequestComputationOutput -> Msg
remoteComputationDoneToMsg result =
    case result of
        Ok backRequestComputationOutput ->
            RemoteComputationDone <|
                Client.convertRequestComputationOutputFromBackToFront backRequestComputationOutput

        Err error ->
            RemoteComputationFailed


parseHeaders : String -> List ( String, String )
parseHeaders headers =
    let
        parseRawHeader : String -> ( String, String )
        parseRawHeader rawHeader =
            case String.split ":" rawHeader of
                [ headerKey, headerValue ] ->
                    ( headerKey, headerValue )

                _ ->
                    ( "", "" )
    in
    String.lines headers |> List.map parseRawHeader


buildRequestToRun : List (Storable NewKeyValue KeyValue) -> Model -> Cmd Msg
buildRequestToRun envKeyValues model =
    let
        request : RequestComputationInput
        request =
            buildRequestComputationInput model

        mkHeader : ( String, String ) -> Http.Header
        mkHeader ( headerKey, headerValue ) =
            Http.header headerKey headerValue

        backRequestComputationInput =
            ( Client.convertRequestComputationInputFromFrontToBack request
            , envKeyValues
                |> List.map latestValueOfStorable
                |> List.map (Tuple.mapSecond Client.convertStringTemplateFromFrontToBack)
                |> Dict.fromList
            )

    in
    Client.postApiRunnerRequestComputation (Runner.runnerUrl model.runnerRunning) backRequestComputationInput remoteComputationDoneToMsg


type DetailedError
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata String


convertResponseStringToResult : Http.Response String -> Result DetailedError ( Http.Metadata, String )
convertResponseStringToResult httpResponse =
    case httpResponse of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ metadata body ->
            Err (BadStatus metadata body)

        Http.GoodStatus_ metadata body ->
            Ok ( metadata, body )


convertResultToResponse : Result DetailedError ( Http.Metadata, String ) -> RequestComputationOutput
convertResultToResponse result =
    case result of
        Err (BadUrl url) ->
            Err (InvalidUrlException url url)

        Err Timeout ->
            Err ResponseTimeout

        Err NetworkError ->
            Err (ConnectionFailure "network error")

        Err (BadStatus metadata body) ->
            Ok { statusCode = metadata.statusCode
               , statusText = metadata.statusText
               , headers = metadata.headers
               , body = body
               }

        Ok ( metadata, body ) ->
            Ok { statusCode = metadata.statusCode
               , statusText = metadata.statusText
               , headers = metadata.headers
               , body = body
               }


expectStringDetailed : (Result DetailedError ( Http.Metadata, String ) -> msg) -> Http.Expect msg
expectStringDetailed msg =
    Http.expectStringResponse msg convertResponseStringToResult


latestValueOfStorable : Storable NewKeyValue KeyValue -> (String, StringTemplate)
latestValueOfStorable storable =
    case storable of
        New { key, value } -> (key, value)
        Saved { key, value } -> (key, value)
        Edited2 _ { key, value } -> (key, value)

-- * view


view : Model -> Maybe Uuid -> Element Msg
view model mFromScenarioId =
    let
        builderView =
            column [ width fill, spacing 10 ]
                [ column [ width fill ]
                    [ row [ width fill, spacing 10 ]
                        [ urlView model
                        ]
                    ]
                , methodView model
                , headersView model
                , bodyView model
                ]
    in
    case model.showResponseView of
        False ->
            column
                [ width fill
                , Background.color white
                , boxShadow
                , padding 20
                ]
                [ goBackToScenarioView mFromScenarioId
                , titleView model
                , el [ width fill ] builderView
                ]

        True ->
            wrappedRow [ alignTop, width fill, spacing 10 ]
                [ column
                    [ width (fillPortion 1)
                    , alignTop
                    , Background.color white
                    , boxShadow
                    , padding 20
                    ]
                    [ goBackToScenarioView mFromScenarioId
                    , titleView model
                    , el [ alignTop ] builderView
                    ]
                , el
                    [ width (fillPortion 1)
                    , Background.color white
                    , boxShadow
                    , alignTop
                    , padding 30
                    ]
                    (responseView model)
                ]


-- ** go back to scenario view


goBackToScenarioView : Maybe Uuid -> Element Msg
goBackToScenarioView mScenarioId =
    case mScenarioId of
        Nothing ->
            none

        Just scenarioId ->
            link []
                { url = href (ScenarioPage (Just scenarioId) Nothing)
                , label = el [ Font.heavy ] <| iconWithTextAndColor "keyboard_backspace" "Go back to scenario" primaryColor
                }


-- ** title


titleView : Model -> Element Msg
titleView model =
    let
        name =
            notEditedValue model.name
    in
    row [ paddingXY 0 10, spacing 10, width fill ]
        [ el [] <| iconWithTextAndColor "label" name secondaryColor
        , el [ alignRight ] (mainActionButtonsView model)
        ]


mainActionButtonsView : Model -> Element Msg
mainActionButtonsView model =
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
            let
                attrs : List (Html.Attribute Msg)
                attrs =
                    Animation.render model.runRequestIconAnimation
            in
            iconWithTextAndColorAndAttr "send" "Run" primaryColor attrs
    in
    row rowParam
        [ case isBuilderDirty model of
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


responseView : Model -> Element Msg
responseView model =
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
    case model.requestComputationResult of
        Nothing ->
            none

        Just (Ok requestComputationOutput) ->
            column [ spacing 10, scrollbars, width fill ]
                [ statusResponseView requestComputationOutput
                , whichResponseButtonView
                      [ ("Body", model.whichResponseView == BodyResponseView, ShowBodyResponseView)
                      , ("Headers", model.whichResponseView == HeaderResponseView, ShowHeaderResponseView)
                      ]
                , case model.whichResponseView of
                      BodyResponseView ->
                          bodyResponseView requestComputationOutput SetHttpBodyResponse

                      HeaderResponseView ->
                          headersResponseView requestComputationOutput SetHttpBody
                ]

        Just (Err httpException) ->
            el errorAttributes (text (httpExceptionToString httpException))



-- ** url


urlView : Model -> Element Msg
urlView model =
    el [ alignLeft, width fill ] <|
        Input.text [ Util.onEnter AskRun ]
            { onChange = UpdateUrl
            , text = editedOrNotEditedValue model.httpUrl
            , placeholder = Just <| Input.placeholder [] (text "myApi.com/path?arg=someArg")
            , label = labelInputView "Url: "
            }



-- ** method


methodView : Model -> Element Msg
methodView model =
    Input.radioRow [ padding 10, spacing 10 ]
        { onChange = SetHttpMethod
        , selected = Just <| editedOrNotEditedValue model.httpMethod
        , label = labelInputView "Method: "
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


headersView : Model -> Element Msg
headersView model =
    let
        headerInputs =
            List.indexedMap (headerView model) (editedOrNotEditedValue model.httpHeaders)

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
        case List.isEmpty (editedOrNotEditedValue model.httpHeaders) of
            True ->
                el [ width fill, spacing 20 ] addHeaderButton

            False ->
                column [ width fill, spacing 20 ]
                    [ text "Header:"
                    , column [ width fill, spacing 10 ] headerInputs
                    , addHeaderButton
                    ]


headerView : Model -> Int -> ( String, String ) -> Element Msg
headerView model idx ( headerKey, headerValue ) =
    row [ width fill, spacing 10 ]
        [ Input.text [ Util.onEnter AskRun ]
            { onChange = UpdateHeaderKey idx
            , text = headerKey
            , placeholder = Nothing
            , label = Input.labelLeft [ centerY ] (text "key: ")
            }
        , Input.text [ Util.onEnter AskRun ]
            { onChange = UpdateHeaderValue idx
            , text = headerValue
            , placeholder = Nothing
            , label = Input.labelLeft [ centerY ] (text "value: ")
            }
        , Input.button [ centerY ]
            { onPress = Just <| DeleteHeader idx
            , label =
                row [ centerX, centerY ]
                    [ deleteIcon
                    ]
            }
        ]



-- ** body


bodyView : Model -> Element Msg
bodyView model =
    Input.multiline []
        { onChange = SetHttpBody
        , text = editedOrNotEditedValue model.httpBody
        , placeholder = Just <| Input.placeholder [] (text "{}")
        , label = labelInputView "Body: "
        , spellcheck = False
        }



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



-- * subscriptions


type alias ModelSubscriptions a =
    { a | runRequestIconAnimation : Animation.State }


subscriptions : ModelSubscriptions a -> Sub Msg
subscriptions model =
    Sub.batch
        [ Animation.subscription Animate [ model.runRequestIconAnimation ]
        ]
