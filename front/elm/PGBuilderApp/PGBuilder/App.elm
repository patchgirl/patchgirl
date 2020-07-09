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
    , dbHost : Editable String
    , dbPort : Editable String
    , dbUser : Editable String
    , dbPassword : Editable String
    , dbName : Editable String
    , sqlQuery : Editable String
    , keyValues : List (Storable NewKeyValue KeyValue)
    , pgComputation : Maybe PgComputation
    , showResponseView : Bool
    , runnerRunning : Bool
    }


-- * message


type Msg
    = UpdateSqlQuery String
    | UpdateHost String
    | UpdatePort String
    | UpdateUser String
    | UpdatePassword String
    | UpdateDbName String
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

        UpdateHost newHost ->
            let
                newModel =
                    { model | dbHost = changeEditedValue newHost model.dbHost }
            in
            (newModel, Cmd.none)

        UpdatePort newPort ->
            let
                newModel =
                    { model | dbPort = changeEditedValue newPort model.dbPort }
            in
            (newModel, Cmd.none)

        UpdateUser newUser ->
            let
                newModel =
                    { model | dbUser = changeEditedValue newUser model.dbUser }
            in
            (newModel, Cmd.none)

        UpdatePassword newPassword ->
            let
                newModel =
                    { model | dbPassword = changeEditedValue newPassword model.dbPassword }
            in
            (newModel, Cmd.none)

        UpdateDbName newDbName ->
            let
                newModel =
                    { model | dbName = changeEditedValue newDbName model.dbName }
            in
            (newModel, Cmd.none)

        AskRun ->
            let
                payload =
                    ( model.keyValues
                            |> List.map latestValueOfStorable
                            |> List.map (Tuple.mapSecond Client.convertStringTemplateFromFrontToBack)
                            |> Dict.fromList
                    , { pgComputationInputSql =
                          editedOrNotEditedValue model.sqlQuery
                              |> stringToTemplate
                              |> Client.convertStringTemplateFromFrontToBack
                      , pgComputationInputPgConnection =
                          { templatedPgConnectionHost =
                                editedOrNotEditedValue model.dbHost |> stringToTemplate |> Client.convertStringTemplateFromFrontToBack
                          , templatedPgConnectionPort =
                              editedOrNotEditedValue model.dbPort |> stringToTemplate |> Client.convertStringTemplateFromFrontToBack
                          , templatedPgConnectionUser =
                              editedOrNotEditedValue model.dbUser |> stringToTemplate |> Client.convertStringTemplateFromFrontToBack
                          , templatedPgConnectionPassword =
                              editedOrNotEditedValue model.dbPassword |> stringToTemplate |> Client.convertStringTemplateFromFrontToBack
                          , templatedPgConnectionDbName =
                              editedOrNotEditedValue model.dbName |> stringToTemplate |> Client.convertStringTemplateFromFrontToBack
                          }
                      }
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
                    , updatePgFileHost = editedOrNotEditedValue model.dbHost
                    , updatePgFilePort = editedOrNotEditedValue model.dbPort
                    , updatePgFileUser = editedOrNotEditedValue model.dbUser
                    , updatePgFileDbName = editedOrNotEditedValue model.dbName
                    , updatePgFilePassword = editedOrNotEditedValue model.dbPassword
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
                        , dbHost = NotEdited (editedOrNotEditedValue model.dbHost)
                        , dbPort = NotEdited (editedOrNotEditedValue model.dbPort)
                        , dbUser = NotEdited (editedOrNotEditedValue model.dbUser)
                        , dbName = NotEdited (editedOrNotEditedValue model.dbName)
                        , dbPassword = NotEdited (editedOrNotEditedValue model.dbPassword)
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
    List.any isDirty
        [ model.dbHost
        , model.dbPort
        , model.dbUser
        , model.dbPassword
        , model.dbName
        , model.sqlQuery
        ]


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
        , case model.showResponseView of
              True ->
                  el [ width (fillPortion 1)
                     , Background.color white
                     , boxShadow
                     , alignTop
                     , padding 30
                     ] (responseView model)

              False ->
                  none
        ]

builderView : Model -> Element Msg
builderView model =
    column [ alignLeft, width fill, spacing 20 ]
        [ row [ spacing 10, width fill ]
              [ el [] <| iconWithTextAndColor "label" (editedOrNotEditedValue model.name) secondaryColor
              , case isBuilderDirty model of
                    True ->
                        Input.button [ alignRight ]
                            { onPress = Just <| AskSave
                            , label =
                                el [ Border.solid
                                   , Border.color secondaryColor
                                   , Border.width 1
                                   , Border.rounded 5
                                   , Background.color secondaryColor
                                   , paddingXY 10 10
                                   ] (iconWithTextAndColorAndAttr "save" "Save" primaryColor [] )
                            }

                    False ->
                        none
              , Input.button [ alignRight ]
                    { onPress = Just <| AskRun
                    , label =
                        el [ Border.solid
                           , Border.color secondaryColor
                           , Border.width 1
                           , Border.rounded 5
                           , Background.color secondaryColor
                           , paddingXY 10 10
                           ] (iconWithTextAndColorAndAttr "send" "Run" primaryColor [] )
                    }
              ]
        , row [ spacing 10 ]
              [ Input.text []
                    { onChange = UpdateHost
                    , text = editedOrNotEditedValue model.dbHost
                    , placeholder = Just <| Input.placeholder [] (text "localhost")
                    , label = labelInputView "Host: "
                    }
              , Input.text []
                  { onChange = UpdatePort
                  , text = editedOrNotEditedValue model.dbPort
                  , placeholder = Just <| Input.placeholder [] (text "5432")
                  , label = labelInputView "Port: "
                  }
              , Input.text []
                  { onChange = UpdateUser
                  , text = editedOrNotEditedValue model.dbUser
                  , placeholder = Just <| Input.placeholder [] (text "postgres")
                  , label = labelInputView "User: "
                  }
              , Input.text []
                  { onChange = UpdatePassword
                  , text = editedOrNotEditedValue model.dbPassword
                  , placeholder = Just <| Input.placeholder [] (text "password")
                  , label = labelInputView "Password: "
                  }
              , Input.text []
                  { onChange = UpdateDbName
                  , text = editedOrNotEditedValue model.dbName
                  , placeholder = Just <| Input.placeholder [] (text "database")
                  , label = labelInputView "Database: "
                  }
              ]
        , Input.text [ Util.onEnter AskRun ]
            { onChange = UpdateSqlQuery
            , text = editedOrNotEditedValue model.sqlQuery
            , placeholder = Just <| Input.placeholder [] (text "SELECT * FROM your_table;")
            , label = labelInputView "SQL: "
            }
        ]

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

                                tableHeaderView : String -> Element Msg
                                tableHeaderView name =
                                    column [ width fill, spacing 10 ]
                                        [ text name
                                        , el [ width fill
                                             , Border.solid
                                             , Border.color black
                                             , Border.width 1
                                             ] none
                                        ]

                            in
                                column [ width fill, spacing 10 ] <|
                                    [ tableHeaderView columnName ] ++ (List.map showPGValue pgValues)

                    in
                        row [ width fill ] <|
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
