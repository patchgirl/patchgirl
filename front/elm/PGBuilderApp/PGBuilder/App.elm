module PGBuilderApp.PGBuilder.App exposing (..)

import Animation
import Api.Converter as Client
import Api.WebGeneratedClient as Client
import Api.RunnerGeneratedClient as Client
import Application.Type exposing (..)
import Dict exposing (Dict)
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
import HttpError exposing(..)
import Interpolator exposing(..)


-- * model


type alias Model =
    { id : Uuid
    , notification : Maybe Notification
    , pgCollectionId : Uuid
    , name : Editable String
    , dbHost : Editable String
    , dbPort : Editable String
    , dbUser : Editable String
    , dbPassword : Editable String
    , dbName : Editable String
    , sqlQuery : Editable String
    , keyValues : List (Storable NewKeyValue KeyValue)
    , pgComputationOutput : Maybe PgComputationOutput
    , showResponseView : Bool
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
    | RemoteComputationDone PgComputationOutput
    | RemoteComputationFailed
    | PrintNotification Notification
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
                    buildPgComputationPayload model

                newMsg =
                    Client.postApiRunnerPgSqlComputation Runner.desktopRunnerUrl payload postPgSqlComputationResultToMsg

                newModel =
                    { model
                        | showResponseView = False
                        , pgComputationOutput = Nothing
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

        RemoteComputationDone pgComputationOutput ->
            let
                newModel =
                    { model
                        | showResponseView = True
                        , pgComputationOutput = Just pgComputationOutput
                    }
            in
            ( newModel, Cmd.none )

        RemoteComputationFailed ->
            let
                newModel =
                    { model
                        | pgComputationOutput = Nothing
                    }
            in
            ( newModel, Cmd.none )

        PrintNotification notification ->
            ( { model | notification = Just notification }, Cmd.none )



-- * util


buildPgComputationPayload : Model -> (Dict String (List Client.Template), Client.PgComputationInput)
buildPgComputationPayload model =
    let
        keyValuesInput =
            model.keyValues
                |> List.map latestValueOfStorable
                |> List.map (Tuple.mapSecond Client.convertStringTemplateFromFrontToBack)
                |> Dict.fromList

        pgComputationInput =
           { pgComputationInputSql =
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
    in
    (keyValuesInput, pgComputationInput)


postPgSqlComputationResultToMsg : Result Http.Error (Result Client.PgError Client.PgComputation) -> Msg
postPgSqlComputationResultToMsg result =
    case result of
        Ok pgComputationOutput ->
            RemoteComputationDone (Client.convertPgComputationOutputFromBackToFront pgComputationOutput)

        Err error ->
            PrintNotification <|
                AlertNotification
                    ("Could not run SQL request. Is <a href=\"" ++ (href (DocumentationPage PatchGirlRunnerAppDoc)) ++ "\">patchgirl-runner</a> running?") (httpErrorToString error)

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
            PrintNotification <| AlertNotification "Could not update the SQL query. Try reloading the page!" (httpErrorToString error)

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
                     ] <| case (model.pgComputationOutput, model.showResponseView) of
                           (Just pgComputationOutput, True) ->
                               responseView pgComputationOutput

                           _ ->
                               none

              False ->
                  none
        ]


-- ** builder


builderView : Model -> Element Msg
builderView model =
    let
        showInterpolatedCredentials : String -> Element Msg
        showInterpolatedCredentials str =
            el [ paddingEach { top = 25, bottom = 0, left = 0, right = 0 } ] <|
                showFullInterpolation "left-arrow-box" model.keyValues str
    in
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
        , row [ spacing 20 ]
              [ row [ width fill, spacing 30 ]
                    [ Input.text []
                          { onChange = UpdateHost
                          , text = editedOrNotEditedValue model.dbHost
                          , placeholder = Just <| Input.placeholder [] (text "localhost")
                          , label = labelInputView "Host: "
                          }
                    , showInterpolatedCredentials (editedOrNotEditedValue model.dbHost)
                    ]
              , row [ width fill, spacing 30 ]
                  [ Input.text []
                        { onChange = UpdatePort
                        , text = editedOrNotEditedValue model.dbPort
                        , placeholder = Just <| Input.placeholder [] (text "5432")
                        , label = labelInputView "Port: "
                        }
                  , showInterpolatedCredentials (editedOrNotEditedValue model.dbPort)
                  ]
              , row [ width fill, spacing 30 ]
                  [ Input.text []
                        { onChange = UpdateUser
                        , text = editedOrNotEditedValue model.dbUser
                        , placeholder = Just <| Input.placeholder [] (text "postgres")
                        , label = labelInputView "User: "
                        }
                  , showInterpolatedCredentials (editedOrNotEditedValue model.dbUser)
                  ]
              , row [ width fill, spacing 30 ]
                  [ Input.text []
                        { onChange = UpdatePassword
                        , text = editedOrNotEditedValue model.dbPassword
                        , placeholder = Just <| Input.placeholder [] (text "password")
                        , label = labelInputView "Password: "
                        }
                  , showInterpolatedCredentials (editedOrNotEditedValue model.dbPassword)
                  ]
              , row [ width fill, spacing 30 ]
                  [ Input.text []
                        { onChange = UpdateDbName
                        , text = editedOrNotEditedValue model.dbName
                        , placeholder = Just <| Input.placeholder [] (text "database")
                        , label = labelInputView "Database: "
                        }
                  , showInterpolatedCredentials (editedOrNotEditedValue model.dbName)
                  ]
              ]
        , wrappedRow [ width fill, spacing 30 ]
            [ Input.multiline [ Util.onEnter AskRun ]
                  { onChange = UpdateSqlQuery
                  , text = editedOrNotEditedValue model.sqlQuery
                  , placeholder = Just <| Input.placeholder [] (text "SELECT * FROM your_table;")
                  , label = labelInputView "SQL: "
                  , spellcheck = False
                  }
            , showOnlyInterpolation model.keyValues (editedOrNotEditedValue model.sqlQuery)
            ]
        ]


-- ** response


responseView : PgComputationOutput -> Element a
responseView pgComputationOutput =
    case pgComputationOutput of
        Err error ->
            text error

        Ok pgComputation ->
            case pgComputation of
                PgCommandOK ->
                    text "Postgresql command Ok"

                PgTuplesOk rows ->
                    let
                        headerRow : List String
                        headerRow =
                            case rows of
                                header :: _ -> List.map Tuple.first header
                                _ -> []

                        rowView : Row -> Element a
                        rowView someRow =
                            let
                                elemView : (String, PgValue) -> Element a
                                elemView (_, value) =
                                    el [ width fill ] (showPGValue value)
                            in
                            row [ width fill, spacing 10 ]
                                <| List.map elemView someRow

                        tableHeaderView : String -> Element a
                        tableHeaderView key =
                            column [ width fill, spacing 10 ]
                                [ text key
                                , el [ width fill
                                     , Border.solid
                                     , Border.color black
                                     , Border.width 1
                                     ] none
                                ]

                    in
                        column [ width fill, spacing 10 ]
                            [ row [ width fill ] (List.map tableHeaderView headerRow)
                            , column [ width fill ] (List.map rowView rows)
                            ]


showPGValue : PgValue -> Element a
showPGValue pgValue =
    case pgValue of
        PgString str -> text str
        PgInt int -> text (String.fromInt int)
        PgFloat float -> text (String.fromFloat float)
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
