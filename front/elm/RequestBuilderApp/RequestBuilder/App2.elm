module RequestBuilderApp.RequestBuilder.App2 exposing (..)

import Animation
import Api.Converter as Client
import Api.WebGeneratedClient as Client
import Api.RunnerGeneratedClient as Client
import Application.Type exposing (..)
import Dict
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
import HttpError exposing(..)
import Interpolator exposing(..)
import Browser.Navigation as Navigation
import RequestBuilderApp.RequestTree.Util as RequestTree
import RequestBuilderApp.RequestTree.App as RequestTree
import Application.Model as Application


-- * model


type alias Model a =
    { a
        | notification : Maybe Notification
        , requestCollection : RequestCollection
        , displayedRequestNodeMenuId : Maybe Uuid
        , displayedRequestId : Maybe Uuid
        , displayedRequestBuilderView : Maybe BuilderView
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , page : Page
        , runnerRunning : Bool
        , navigationKey : Navigation.Key
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
    | PrintNotification Notification
    | AskSave
    | SaveSuccessfully
    | Animate Animation.Msg
    | ShowBodyResponseView
    | ShowHeaderResponseView

type DetailedError
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Http.Metadata String


-- * update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case getRequestNode model of
        Nothing ->
            (model, Cmd.none)

        Just requestNode ->
            case requestNode of
                RequestFolder requestFolderRecord ->
                    (model, Cmd.none)

                RequestFile requestFileRecord ->
                    let
                        (updatedModel, newRequestFileRecord, newMsg) =
                            updateRequestFileRecord model requestFileRecord msg

                        (RequestCollection requestCollectionId requestNodes) =
                            updatedModel.requestCollection

                        newBuilderTree =
                            requestNodes
                                |> List.map (RequestTree.modifyRequestNode (getRequestNodeId requestNode) (always (RequestFile newRequestFileRecord)))

                        newModel =
                            { updatedModel
                                | requestCollection = RequestCollection requestCollectionId newBuilderTree
                            }
                    in
                    ( newModel, Cmd.none )


-- ** update request file record


updateRequestFileRecord : Model a -> RequestFileRecord -> Msg -> (Model a, RequestFileRecord, Cmd Msg)
updateRequestFileRecord model requestFileRecord msg =
    case msg of
        UpdateUrl newHttpUrl ->
            let
                newRequestFileRecord =
                    { requestFileRecord | httpUrl = changeEditedValue newHttpUrl requestFileRecord.httpUrl }
            in
            (model, newRequestFileRecord, Cmd.none )

        SetHttpMethod newMethod ->
            let
                newRequestFileRecord =
                    { requestFileRecord | httpMethod = changeEditedValue newMethod requestFileRecord.httpMethod }
            in
            (model, newRequestFileRecord, Cmd.none )

        UpdateHeaderKey idx newKey ->
            let
                editedHttpHeaders =
                    List.updateAt idx (\( _, value ) -> ( newKey, value )) (editedOrNotEditedValue requestFileRecord.httpHeaders)

                newHttpHeaders =
                    changeEditedValue editedHttpHeaders requestFileRecord.httpHeaders

                newRequestFileRecord =
                    { requestFileRecord | httpHeaders = newHttpHeaders }
            in
            (model, newRequestFileRecord, Cmd.none )

        UpdateHeaderValue idx newValue ->
            let
                editedHttpHeaders =
                    List.updateAt idx (\( key, _ ) -> ( key, newValue )) (editedOrNotEditedValue requestFileRecord.httpHeaders)

                newHttpHeaders =
                    changeEditedValue editedHttpHeaders requestFileRecord.httpHeaders

                newRequestFileRecord =
                    { requestFileRecord | httpHeaders = newHttpHeaders }
            in
            (model, newRequestFileRecord, Cmd.none )

        SetHttpBody httpBody ->
            let
                newRequestFileRecord =
                    { requestFileRecord | httpBody = changeEditedValue httpBody requestFileRecord.httpBody }
            in
            (model, newRequestFileRecord, Cmd.none )

        AskSave ->
            let
                payload : Client.UpdateRequestFile
                payload =
                    { updateRequestFileName = editedOrNotEditedValue requestFileRecord.name
                    , updateRequestFileHttpUrl = editedOrNotEditedValue requestFileRecord.httpUrl
                    , updateRequestFileHttpMethod = Client.convertMethodFromFrontToBack (editedOrNotEditedValue requestFileRecord.httpMethod)
                    , updateRequestFileHttpHeaders = editedOrNotEditedValue requestFileRecord.httpHeaders
                    , updateRequestFileHttpBody = editedOrNotEditedValue requestFileRecord.httpBody
                    }

                (RequestCollection requestCollectionId _) =
                    model.requestCollection

                newMsg =
                    Client.putApiRequestCollectionByRequestCollectionIdByRequestNodeId "" "" requestCollectionId requestFileRecord.id payload updateRequestFileResultToMsg
            in
            ( model, requestFileRecord, newMsg )

        SaveSuccessfully ->
            let
                newRequestFileRecord =
                    { requestFileRecord
                        | name = NotEdited (editedOrNotEditedValue requestFileRecord.name)
                        , httpUrl = NotEdited (editedOrNotEditedValue requestFileRecord.httpUrl)
                        , httpMethod = NotEdited (editedOrNotEditedValue requestFileRecord.httpMethod)
                        , httpHeaders = NotEdited (editedOrNotEditedValue requestFileRecord.httpHeaders)
                        , httpBody = NotEdited (editedOrNotEditedValue requestFileRecord.httpBody)
                    }
            in
            (model, newRequestFileRecord, Cmd.none )

        AskRun ->
            let
                keyValuesToRun =
                    Application.getEnvironmentKeyValuesToRun model

                newMsg =
                    buildRequestToRun keyValuesToRun model requestFileRecord

                newRunRequestIconAnimation =
                    Animation.interrupt
                        [ Animation.loop
                            [ Animation.to [ Animation.rotate (Animation.turn 1) ]
                            , Animation.set [ Animation.rotate (Animation.turn 0) ]
                            ]
                        ]
                        requestFileRecord.runRequestIconAnimation

                newRequestFileRecord =
                    { requestFileRecord
                        | showResponseView = False
                        , requestComputationResult = Nothing
                        , runRequestIconAnimation = newRunRequestIconAnimation
                    }
            in
            (model, newRequestFileRecord, newMsg )

        RemoteComputationDone remoteComputationResult ->
            let
                newRunRequestIconAnimation =
                    Animation.interrupt
                        [ Animation.to [ Animation.rotate (Animation.turn 1) ]
                        , Animation.set [ Animation.rotate (Animation.turn 0) ]
                        ]
                        requestFileRecord.runRequestIconAnimation

                newRequestFileRecord =
                    { requestFileRecord
                        | showResponseView = True
                        , requestComputationResult = Just remoteComputationResult
                        , runRequestIconAnimation = newRunRequestIconAnimation
                    }
            in
            (model, newRequestFileRecord, Cmd.none )

        LocalComputationDone result ->
            let
                newRequestFileRecord =
                    { requestFileRecord
                        | requestComputationResult = Just (convertResultToResponse result)
                    }
            in
            (model, newRequestFileRecord, Cmd.none )

        SetHttpBodyResponse newBody ->
            case requestFileRecord.requestComputationResult of
                Just (Ok response) ->
                    let
                        newRequestComputation =
                            { response | body = newBody }

                        newRequestFileRecord =
                            { requestFileRecord
                                | requestComputationResult = Just (Ok newRequestComputation)
                            }
                    in
                    (model, newRequestFileRecord, Cmd.none )

                _ ->
                    (model, requestFileRecord, Cmd.none )

        AddHeaderInput ->
            let
                newHttpHeaders =
                    Edited (notEditedValue requestFileRecord.httpHeaders) (editedOrNotEditedValue requestFileRecord.httpHeaders ++ [ ( "", "" ) ])

                newRequestFileRecord =
                    { requestFileRecord | httpHeaders = newHttpHeaders }
            in
            (model, newRequestFileRecord, Cmd.none )

        DeleteHeader idx ->
            let
                editedHttpHeaders =
                    List.removeAt idx (editedOrNotEditedValue requestFileRecord.httpHeaders)

                newHttpHeaders =
                    changeEditedValue editedHttpHeaders requestFileRecord.httpHeaders

                newRequestFileRecord =
                    { requestFileRecord | httpHeaders = newHttpHeaders }
            in
            (model, newRequestFileRecord, Cmd.none )

        Animate subMsg ->
            let
                newRequestFileRecord =
                    { requestFileRecord
                        | runRequestIconAnimation =
                            Animation.update subMsg requestFileRecord.runRequestIconAnimation
                    }
            in
            (model, newRequestFileRecord, Cmd.none )

        ShowBodyResponseView ->
            let
                newRequestFileRecord =
                    { requestFileRecord | whichResponseView = BodyResponseView }
            in
            (model, newRequestFileRecord, Cmd.none )

        ShowHeaderResponseView ->
            let
                newRequestFileRecord =
                    { requestFileRecord | whichResponseView = HeaderResponseView }
            in
            (model, newRequestFileRecord, Cmd.none )

        PrintNotification notification ->
            let
                newRunRequestIconAnimation =
                    Animation.interrupt
                        [ Animation.to [ Animation.rotate (Animation.turn 1) ]
                        , Animation.set [ Animation.rotate (Animation.turn 0) ]
                        ]
                        requestFileRecord.runRequestIconAnimation

                newRequestFileRecord =
                    { requestFileRecord
                        | requestComputationResult = Nothing
                        , runRequestIconAnimation = newRunRequestIconAnimation
                    }
            in
            (model, newRequestFileRecord, Cmd.none )


-- * util


getRequestNode : Model a -> Maybe RequestNode
getRequestNode model =
    let
        (RequestCollection requestCollectionId requestNodes) =
            model.requestCollection
    in
    model.displayedRequestBuilderView
        |> Maybe.map getBuilderId
        |> Maybe.andThen (RequestTree.findNode requestNodes)


isBuilderDirty : RequestFileRecord -> Bool
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
            PrintNotification <| AlertNotification "" (httpErrorToString error)

remoteComputationDoneToMsg : Result Http.Error Client.RequestComputationOutput -> Msg
remoteComputationDoneToMsg result =
    case result of
        Ok backRequestComputationOutput ->
            RemoteComputationDone <|
                Client.convertRequestComputationOutputFromBackToFront backRequestComputationOutput

        Err error ->
            PrintNotification <| AlertNotification "Could not run HTTP request" (httpErrorToString error)

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

buildRequestToRun : List (Storable NewKeyValue KeyValue) -> Model a -> RequestFileRecord -> Cmd Msg
buildRequestToRun envKeyValues model requestFileRecord =
    let
        request : RequestComputationInput
        request =
            buildRequestComputationInput requestFileRecord

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


-- * view


view : Model a -> Maybe Uuid -> Element Msg
view model mFromScenarioId =
    none
