module PGBuilderApp.PGBuilder.App exposing (..)

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
import StringTemplate exposing(..)


-- * model


type alias Model =
    { id : Uuid
    , pgCollectionId : Uuid
    , name : Editable String
    , sqlQuery : Editable String
    , keyValues : List (Storable NewKeyValue KeyValue)
    , pgComputation : Maybe PgComputation
    , showResponseView : Bool
    , runnerRunning : Bool
    }


-- * message


type Msg
    = UpdateSqlQuery String
    | AskRun
    | RemoteComputationDone PgComputation
    | RemoteComputationFailed
    | ServerError
    | AskSave
    | SaveSuccessfully


-- * update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateSqlQuery newSqlQuery ->
            let
                newModel =
                    { model | sqlQuery = changeEditedValue newSqlQuery model.sqlQuery }
            in
            (newModel, Cmd.none)

        AskRun ->
            let
                payload =
                    ( editedOrNotEditedValue model.sqlQuery
                      |> stringToTemplate
                      |> Client.convertStringTemplateFromFrontToBack
                    , model.keyValues
                      |> List.map latestValueOfStorable
                      |> List.map (Tuple.mapSecond Client.convertStringTemplateFromFrontToBack)
                      |> Dict.fromList
                    )

                newMsg =
                    Client.postApiRunnerPgSqlComputation Runner.desktopRunnerUrl payload postPgSqlComputationResultToMsg

                newModel =
                    { model
                        | showResponseView = False
                        , pgComputation = Nothing
                    }
            in
                (newModel, newMsg)

        AskSave ->
            let
                payload : Client.UpdatePgFile
                payload =
                    { updatePgFileName = editedOrNotEditedValue model.name
                    , updatePgFileSql = editedOrNotEditedValue model.sqlQuery
                    }

                newMsg =
                    Client.putApiPgCollectionByPgCollectionIdByPgNodeId "" "" model.pgCollectionId model.id payload updatePgFileResultToMsg
            in
            ( model, newMsg )

        SaveSuccessfully ->
            let
                newModel =
                    { model
                        | name = NotEdited (editedOrNotEditedValue model.name)
                        , sqlQuery = NotEdited (editedOrNotEditedValue model.sqlQuery)
                    }
            in
            ( newModel, Cmd.none )

        RemoteComputationDone remoteComputationResult ->
            let
                newModel =
                    { model
                        | showResponseView = True
                        , pgComputation = Just remoteComputationResult
                    }
            in
            ( newModel, Cmd.none )

        RemoteComputationFailed ->
            let
                newModel =
                    { model
                        | pgComputation = Nothing
                    }
            in
            ( newModel, Cmd.none )

        ServerError ->
            (Debug.log "server error" model, Cmd.none)


-- * util



postPgSqlComputationResultToMsg : Result Http.Error Client.PgComputation -> Msg
postPgSqlComputationResultToMsg result =
    case result of
        Ok pgComputation ->
            RemoteComputationDone (Client.convertPgComputationFromBackToFront pgComputation)

        Err error ->
            ServerError

isBuilderDirty : Model -> Bool
isBuilderDirty model =
    isDirty model.sqlQuery

updatePgFileResultToMsg : Result Http.Error () -> Msg
updatePgFileResultToMsg result =
    case result of
        Ok () ->
            SaveSuccessfully

        Err error ->
            ServerError

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


latestValueOfStorable : Storable NewKeyValue KeyValue -> (String, StringTemplate)
latestValueOfStorable storable =
    case storable of
        New { key, value } -> (key, value)
        Saved { key, value } -> (key, value)
        Edited2 _ { key, value } -> (key, value)


-- * view


view : Model -> Element Msg
view model =
    wrappedRow [ alignTop, width fill, spacing 10 ]
        [ el [ width (fillPortion 1)
             , alignTop
             , Background.color white
             , boxShadow
             , padding 20
             ] (builderView model)
        , el [ width (fillPortion 1)
             , Background.color white
             , boxShadow
             , alignTop
             , padding 30
             ] (responseView model)
        ]

builderView : Model -> Element Msg
builderView model =
    el [ alignLeft, width fill ] <|
        Input.text [ Util.onEnter AskRun ]
            { onChange = UpdateSqlQuery
            , text = editedOrNotEditedValue model.sqlQuery
            , placeholder = Just <| Input.placeholder [] (text "SELECT * FROM your_table;")
            , label = labelInputView "Postgres SQL: "
            }

responseView : Model -> Element Msg
responseView model =
    case (model.showResponseView, model.pgComputation) of
        (True, Just result) ->
            case result of
                PgError error -> text error
                PgCommandOK -> text "Postgresql command Ok"
                PgTuplesOk columns ->
                    let
                        columnView : Col -> Element Msg
                        columnView col =
                            let
                                (Col columnName pgValues) = col
                            in
                                column [] <|
                                    (text columnName) :: List.map showPGValue pgValues

                    in
                        row [] <|
                            List.map columnView columns


        _ ->
            none

showPGValue : PgValue -> Element Msg
showPGValue pgValue =
    case pgValue of
        PgString str -> text str
        PgInt int -> text (String.fromInt int)
        PgBool bool -> text "true"
        PgNull -> text "NULL"


-- * util

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
