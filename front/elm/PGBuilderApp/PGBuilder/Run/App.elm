module PGBuilderApp.PGBuilder.Run.App exposing (..)

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
import Html as Html
import Html.Attributes as Html
import Html.Events as Html
import Http
import Util exposing (..)
import Uuid exposing (Uuid)
import Page exposing(..)
import HttpError exposing(..)
import RequestBuilderApp.RequestTree.Util as RequestTree
import PGBuilderApp.PGTree.App as PgTree
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


-- * model


type alias Model a =
    { a
        | notification : Maybe Notification
        , pgCollection : PgCollection
        , pgNewNode : NewNode
        , displayedPgBuilderView : BuilderView Uuid
        , navigationKey : Navigation.Key
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , runnerRunning : Bool
        , page : Page
    }


-- * msg


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


update : Msg -> Model a -> PgFileRecord -> (Model a, PgFileRecord, Cmd Msg)
update msg model file =
    case msg of
        UpdateSqlQuery newSqlQuery ->
            let
                newFile =
                    { file | sql = changeEditedValue newSqlQuery file.sql }
            in
            (model, newFile, Cmd.none)

        UpdateHost newHost ->
            let
                newFile =
                    { file | dbHost = changeEditedValue newHost file.dbHost }
            in
            (model, newFile, Cmd.none)

        UpdatePort newPort ->
            let
                newFile =
                    { file | dbPort = changeEditedValue newPort file.dbPort }
            in
            (model, newFile, Cmd.none)

        UpdateUser newUser ->
            let
                newFile =
                    { file | dbUser = changeEditedValue newUser file.dbUser }
            in
            (model, newFile, Cmd.none)

        UpdatePassword newPassword ->
            let
                newFile =
                    { file | dbPassword = changeEditedValue newPassword file.dbPassword }
            in
            (model, newFile, Cmd.none)

        UpdateDbName newDbName ->
            let
                newFile =
                    { file | dbName = changeEditedValue newDbName file.dbName }
            in
            (model, newFile, Cmd.none)

        AskRun ->
            let
                payload =
                    buildPgComputationPayload file model

                newMsg =
                    Client.postApiRunnerPgSqlComputation Runner.desktopRunnerUrl payload postPgSqlComputationResultToMsg

                newFile =
                    { file
                        | showResponseView = False
                        , pgComputationOutput = Nothing
                    }
            in
                (model, newFile, newMsg)

        AskSave ->
            let
                payload : Client.UpdatePgFile
                payload =
                    { updatePgFileName = editedOrNotEditedValue file.name
                    , updatePgFileSql = editedOrNotEditedValue file.sql
                    , updatePgFileHost = editedOrNotEditedValue file.dbHost
                    , updatePgFilePort = editedOrNotEditedValue file.dbPort
                    , updatePgFileUser = editedOrNotEditedValue file.dbUser
                    , updatePgFileDbName = editedOrNotEditedValue file.dbName
                    , updatePgFilePassword = editedOrNotEditedValue file.dbPassword
                    }

                (PgCollection pgCollectionId _) =
                    model.pgCollection

                newMsg =
                    Client.putApiPgCollectionByPgCollectionIdByPgNodeId "" "" pgCollectionId file.id payload updatePgFileResultToMsg
            in
            ( model, file, newMsg )

        SaveSuccessfully ->
            let
                newFile =
                    { file
                        | name = NotEdited (editedOrNotEditedValue file.name)
                        , sql = NotEdited (editedOrNotEditedValue file.sql)
                        , dbHost = NotEdited (editedOrNotEditedValue file.dbHost)
                        , dbPort = NotEdited (editedOrNotEditedValue file.dbPort)
                        , dbUser = NotEdited (editedOrNotEditedValue file.dbUser)
                        , dbName = NotEdited (editedOrNotEditedValue file.dbName)
                        , dbPassword = NotEdited (editedOrNotEditedValue file.dbPassword)
                    }
            in
            ( model, newFile, Cmd.none )

        RemoteComputationDone pgComputationOutput ->
            let
                newFile =
                    { file
                        | showResponseView = True
                        , pgComputationOutput = Just pgComputationOutput
                    }
            in
            ( model, newFile, Cmd.none )

        RemoteComputationFailed ->
            let
                newFile =
                    { file
                        | pgComputationOutput = Nothing
                    }
            in
            ( model, newFile, Cmd.none )

        PrintNotification notification ->
            let
                newModel =
                    { model | notification = Just notification }
            in
            ( newModel, file, Cmd.none )



-- * util


keyValuesToRun : Model a -> List (Storable NewKeyValue KeyValue)
keyValuesToRun model =
    Application.getEnvironmentKeyValuesToRun model


buildPgComputationPayload : PgFileRecord -> Model a -> (Dict String (List Client.Template), Client.PgComputationInput)
buildPgComputationPayload file model =
    let
        keyValuesInput =
            keyValuesToRun model
                |> List.map latestValueOfStorable
                |> List.map (Tuple.mapSecond Client.convertStringTemplateFromFrontToBack)
                |> Dict.fromList

        pgComputationInput =
           { pgComputationInputSql =
               editedOrNotEditedValue file.sql
                   |> stringToTemplate
                   |> Client.convertStringTemplateFromFrontToBack
           , pgComputationInputPgConnection =
               { templatedPgConnectionHost =
                   editedOrNotEditedValue file.dbHost |> stringToTemplate |> Client.convertStringTemplateFromFrontToBack
               , templatedPgConnectionPort =
                   editedOrNotEditedValue file.dbPort |> stringToTemplate |> Client.convertStringTemplateFromFrontToBack
               , templatedPgConnectionUser =
                   editedOrNotEditedValue file.dbUser |> stringToTemplate |> Client.convertStringTemplateFromFrontToBack
               , templatedPgConnectionPassword =
                   editedOrNotEditedValue file.dbPassword |> stringToTemplate |> Client.convertStringTemplateFromFrontToBack
               , templatedPgConnectionDbName =
                   editedOrNotEditedValue file.dbName |> stringToTemplate |> Client.convertStringTemplateFromFrontToBack
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

isBuilderDirty : PgFileRecord -> Bool
isBuilderDirty file =
    List.any isDirty
        [ file.dbHost
        , file.dbPort
        , file.dbUser
        , file.dbPassword
        , file.dbName
        , file.sql
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


view : Model a -> PgFileRecord -> Element Msg
view model file =
    wrappedRow [ alignTop, width fill, spacing 10 ]
        [ el ( box  [ width (fillPortion 1), alignTop, padding 20 ] ) <|
              builderView model file
        , case file.showResponseView of
              True ->
                  el ( box [ width (fillPortion 1), alignTop, padding 30 ] ) <|
                      case (file.pgComputationOutput, file.showResponseView) of
                           (Just pgComputationOutput, True) ->
                               responseView pgComputationOutput

                           _ ->
                               none

              False ->
                  none
        ]


-- ** builder


builderView : Model a -> PgFileRecord -> Element Msg
builderView model file =
    let
        showInterpolatedCredentials : String -> Element Msg
        showInterpolatedCredentials str =
            el [ paddingEach { top = 25, bottom = 0, left = 0, right = 0 } ] <|
                showFullInterpolation "left-arrow-box" (keyValuesToRun model) str
    in
    column [ alignLeft, width fill, spacing 20 ]
        [ row [ spacing 10, width fill ]
              [ el [] <| iconWithTextAndColor "label" (editedOrNotEditedValue file.name) secondaryColor
              , case isBuilderDirty file of
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
                          , text = editedOrNotEditedValue file.dbHost
                          , placeholder = Just <| Input.placeholder [] (text "localhost")
                          , label = labelInputView "Host: "
                          }
                    , showInterpolatedCredentials (editedOrNotEditedValue file.dbHost)
                    ]
              , row [ width fill, spacing 30 ]
                  [ Input.text []
                        { onChange = UpdatePort
                        , text = editedOrNotEditedValue file.dbPort
                        , placeholder = Just <| Input.placeholder [] (text "5432")
                        , label = labelInputView "Port: "
                        }
                  , showInterpolatedCredentials (editedOrNotEditedValue file.dbPort)
                  ]
              , row [ width fill, spacing 30 ]
                  [ Input.text []
                        { onChange = UpdateUser
                        , text = editedOrNotEditedValue file.dbUser
                        , placeholder = Just <| Input.placeholder [] (text "postgres")
                        , label = labelInputView "User: "
                        }
                  , showInterpolatedCredentials (editedOrNotEditedValue file.dbUser)
                  ]
              , row [ width fill, spacing 30 ]
                  [ Input.text []
                        { onChange = UpdatePassword
                        , text = editedOrNotEditedValue file.dbPassword
                        , placeholder = Just <| Input.placeholder [] (text "password")
                        , label = labelInputView "Password: "
                        }
                  , showInterpolatedCredentials (editedOrNotEditedValue file.dbPassword)
                  ]
              , row [ width fill, spacing 30 ]
                  [ Input.text []
                        { onChange = UpdateDbName
                        , text = editedOrNotEditedValue file.dbName
                        , placeholder = Just <| Input.placeholder [] (text "database")
                        , label = labelInputView "Database: "
                        }
                  , showInterpolatedCredentials (editedOrNotEditedValue file.dbName)
                  ]
              ]
        , wrappedRow [ width fill, spacing 30 ]
            [ Input.multiline [ Util.onEnter AskRun ]
                  { onChange = UpdateSqlQuery
                  , text = editedOrNotEditedValue file.sql
                  , placeholder = Just <| Input.placeholder [] (text "SELECT * FROM your_table;")
                  , label = labelInputView "SQL: "
                  , spellcheck = False
                  }
            , showOnlyInterpolation (keyValuesToRun model) (editedOrNotEditedValue file.sql)
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
