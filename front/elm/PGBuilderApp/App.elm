module PGBuilderApp.App exposing (..)

import Application.Model as Application
import Application.Type exposing (..)
import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import Html as Html
import Html.Attributes as Html
import Html.Events as Html
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Json
import List.Extra as List
import Page exposing (..)
import PGBuilderApp.PGBuilder.App as PGBuilder
import PGBuilderApp.PGTree.App as PgTree
import PGBuilderApp.PGTree.Util as PgTree
import Util exposing (..)
import Uuid exposing (Uuid)
import Api.RunnerGeneratedClient as Client
import Browser.Navigation as Navigation
import Banner exposing (..)


-- * model


type alias Model a =
    { a
        | pgCollection : PgCollection
        , notification : Maybe Notification
        , displayedPgNodeMenuId : Maybe Uuid
        , displayedPgId : Maybe Uuid
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , page : Page
        , runnerRunning : Bool
        , navigationKey : Navigation.Key
    }


-- * message


type Msg
    = BuilderMsg PGBuilder.Msg
    | TreeMsg PgTree.Msg
    | EnvSelectionMsg Int


-- * update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        EnvSelectionMsg idx ->
            let
                newModel =
                    { model | selectedEnvironmentToRunIndex = Just idx }
            in
            ( newModel, Cmd.none )

        TreeMsg subMsg ->
            let
                ( newModel, newSubMsg ) =
                    PgTree.update subMsg model
            in
            ( newModel, Cmd.map TreeMsg newSubMsg )

        BuilderMsg subMsg ->
            case getBuilder model of
                Just builder ->
                    let
                        (PgCollection pgCollectionId pgNodes) =
                            model.pgCollection

                        ( newBuilder, newSubMsg ) =
                            PGBuilder.update subMsg builder

                        newBuilderTree =
                            List.map (PgTree.modifyPgNode builder.id (changeFileBuilder newBuilder)) pgNodes

                        newModel =
                            { model
                                | pgCollection = PgCollection pgCollectionId newBuilderTree
                                , notification = newBuilder.notification
                            }

                        newMsg =
                            Cmd.map BuilderMsg newSubMsg
                    in
                    ( newModel, newMsg )

                _ ->
                    ( model, Cmd.none )


-- * util


getBuilder : Model a -> Maybe PGBuilder.Model
getBuilder model =
    let
        (PgCollection pgCollectionId pgNodes) =
            model.pgCollection

        mFile : Maybe PgFileRecord
        mFile =
            Maybe.andThen (PgTree.findFile pgNodes) model.displayedPgId
    in
    case ( model.displayedPgId, mFile ) of
        ( Just _, Just file ) ->
            let
                keyValuesToRun =
                    Application.getEnvironmentKeyValuesToRun model
            in
            Just (convertFromFileToBuilder file pgCollectionId keyValuesToRun model.notification)

        _ ->
            Nothing


convertFromFileToBuilder : PgFileRecord -> Uuid -> List (Storable NewKeyValue KeyValue) -> Maybe Notification -> PGBuilder.Model
convertFromFileToBuilder file pgCollectionId keyValuesToRun notification =
    { id = file.id
    , pgCollectionId = pgCollectionId
    , notification = notification
    , keyValues = keyValuesToRun
    , name = file.name
    , sqlQuery = file.sql
    , dbHost = file.dbHost
    , dbPort = file.dbPort
    , dbUser = file.dbUser
    , dbPassword = file.dbPassword
    , dbName = file.dbName
    , pgComputationOutput = file.pgComputationOutput
    , showResponseView = file.showResponseView
    }

convertFromBuilderToFile : PGBuilder.Model -> PgFileRecord
convertFromBuilderToFile builder =
    { id = builder.id
    , name = builder.name
    , dbHost = builder.dbHost
    , dbPort = builder.dbPort
    , dbUser = builder.dbUser
    , dbPassword = builder.dbPassword
    , dbName = builder.dbName
    , sql = builder.sqlQuery
    , pgComputationOutput = builder.pgComputationOutput
    , showResponseView = builder.showResponseView
    }

changeFileBuilder : PGBuilder.Model -> PgNode -> PgNode
changeFileBuilder builder node =
    case node of
        PgFolder f ->
            PgFolder f

        PgFile f ->
            PgFile (convertFromBuilderToFile builder)


-- * view


view : Model a -> Element Msg
view model =
    column [ width fill, spacing 10 ]
        [ if not model.runnerRunning then enableRunnerBanner else none
        , wrappedRow
              [ width fill
              , paddingXY 10 0
              , spacing 10
              ]
              [ column ( box [ alignTop
                             , spacing 20
                             , centerX
                             , padding 20
                             , width (fillPortion 1)
                             ]
                       )
                    [ el [] <| envSelectionView <| List.map .name model.environments
                    , el [ paddingXY 10 0 ] <| map TreeMsg (PgTree.view model)
                    ]
              , builderView model
              ]
        ]


envSelectionView : List (Editable String) -> Element Msg
envSelectionView environmentNames =
    let
        entryView : Int -> Editable String -> Html.Html Msg
        entryView idx envName =
            Html.option [ Html.value (String.fromInt idx) ] [ Html.text (editedOrNotEditedValue envName) ]
    in
    html <|
        Html.div []
            [ Html.label [] [ Html.text "Env: " ]
            , Html.select [ Html.on "change" (Json.map EnvSelectionMsg targetValueIntParse) ]
                (List.indexedMap entryView environmentNames)
            ]

builderView : Model a -> Element Msg
builderView model =
    case getBuilder model of
        Just builder ->
            el [ width (fillPortion 9)
               , height fill
               , alignTop
               ]
                (map BuilderMsg (PGBuilder.view builder))

        Nothing ->
            el [ width (fillPortion 9)
               , centerX, centerY, alignTop
               ]
                <| el (box [ centerX
                           , alignTop
                           , padding 20
                           , spacing 10
                           ]
                      ) (text "No postgres sql request selected")
