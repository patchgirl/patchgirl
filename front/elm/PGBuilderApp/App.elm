module PGBuilderApp.App exposing (..)

import Application.Model as Application
import Application.Type exposing (..)
import Element exposing (..)
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



-- * model


type alias Model a =
    { a
        | pgCollection : PgCollection
        , displayedPgNodeMenuId : Maybe Uuid
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Int
        , page : Page
        , runnerRunning : Bool
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
                            }

                        newMsg =
                            Cmd.map BuilderMsg newSubMsg
                    in
                    ( newModel, newMsg )

                _ ->
                    ( model, Cmd.none )


-- * util


getSelectedBuilderId : Model a -> Maybe Uuid
getSelectedBuilderId model =
    case model.page of
        PgPage (Just id) ->
            Just id

        _ ->
            Nothing

getBuilder : Model a -> Maybe PGBuilder.Model
getBuilder model =
    let
        (PgCollection pgCollectionId pgNodes) =
            model.pgCollection

        mFile : Maybe PgFileRecord
        mFile =
            Maybe.andThen (PgTree.findFile pgNodes) (getSelectedBuilderId model)
    in
    case ( getSelectedBuilderId model, mFile ) of
        ( Just _, Just file ) ->
            let
                keyValuesToRun =
                    Application.getEnvironmentKeyValuesToRun model
            in
            Just (convertFromFileToBuilder file pgCollectionId keyValuesToRun model)

        _ ->
            Nothing


convertFromFileToBuilder : PgFileRecord -> Uuid -> List (Storable NewKeyValue KeyValue) -> Model a -> PGBuilder.Model
convertFromFileToBuilder file pgCollectionId keyValuesToRun model =
    { id = file.id
    , pgCollectionId = pgCollectionId
    , keyValues = keyValuesToRun
    , name = file.name
    , sqlQuery = file.sql
    , pgComputation = file.pgComputation
    , showResponseView = file.showResponseView
    , runnerRunning = model.runnerRunning
    }

convertFromBuilderToFile : PGBuilder.Model -> PgFileRecord
convertFromBuilderToFile builder =
    { id = builder.id
    , name = builder.name
    , sql = builder.sqlQuery
    , pgComputation = builder.pgComputation
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


view : Model a -> Maybe Uuid -> Element Msg
view model mId =
    wrappedRow
        [ width fill
        , paddingXY 10 0
        , spacing 10
        ]
        [ column
            [ alignTop
            , spacing 20
            , centerX
            , padding 20
            , width (fillPortion 1)
            , Background.color white
            , boxShadow
            ]
            [ el [] <| envSelectionView <| List.map .name model.environments
            , el [ paddingXY 10 0 ] <| map TreeMsg (PgTree.view model)
            ]
        , builderView model mId
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

builderView : Model a -> Maybe Uuid -> Element Msg
builderView model mId =
    case getBuilder model of
        Just builder ->
            el [ width (fillPortion 9)
               , width fill
               , height fill
               , alignTop
               ]
                (map BuilderMsg (PGBuilder.view builder))

        Nothing ->
            el [ width (fillPortion 9)
               , centerX, centerY, alignTop
               ]
                <| el ( [ centerX
                        , alignTop
                        , padding 20
                        , spacing 10
                        ] ++ boxAttrs
                      ) (text "No postgres sql request selected")
