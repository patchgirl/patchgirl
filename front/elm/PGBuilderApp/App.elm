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
import PGBuilderApp.PGTree.App as PGTree
import PGBuilderApp.PGTree.Util as PGTree
import Util exposing (..)
import Uuid exposing (Uuid)
import Api.RunnerGeneratedClient as Client



-- * model


type alias Model a =
    { a
        | page : Page
        , environments : List Environment
        , sqlQuery : Editable String
        , pgComputation : Client.PGComputation
        , runnerRunning : Bool
    }



-- * message


type Msg
    = BuilderMsg PGBuilder.Msg
    | TreeMsg PGTree.Msg
    | EnvSelectionMsg Int



-- * update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        BuilderMsg subMsg ->
            let
                (newModel, newSubMsg) =
                    PGBuilder.update subMsg { sqlQuery = model.sqlQuery, runnerRunning = model.runnerRunning, pgComputation = model.pgComputation }
            in
            ( { model | sqlQuery = newModel.sqlQuery, pgComputation = newModel.pgComputation }, Cmd.map BuilderMsg newSubMsg)

        _ ->
            (model, Cmd.none)



-- * util


getSelectedBuilderId : Model a -> Maybe Uuid
getSelectedBuilderId model =
    case model.page of
        PGPage (Just id) ->
            Just id

        _ ->
            Nothing


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
            , el [ paddingXY 10 0 ] <| map TreeMsg (PGTree.view model)
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
    case mId of
        Just id ->
            el [ width (fillPortion 9)
               , width fill
               , height fill
               , alignTop
               ] <| map BuilderMsg (PGBuilder.view { sqlQuery = model.sqlQuery, runnerRunning = model.runnerRunning, pgComputation = model.pgComputation })

        Nothing ->
            el [ width (fillPortion 9)
               , centerX, centerY, alignTop
               ]
                <| el ( [ centerX
                        , alignTop
                        , padding 20
                        , spacing 10
                        ] ++ boxAttrs
                      ) (text "No request selected")