module PGBuilderApp.App exposing (..)

import Application.Model as Application
import Application.Type exposing (..)
import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import Html
import Html.Attributes as Html
import Html.Events as Html
import Html.Events.Extra exposing (targetValueIntParse)
import Json.Decode as Json
import List.Extra as List
import Page exposing (..)
import PGBuilderApp.PGBuilder.App as PgBuilder
import PGBuilderApp.PGTree.App as Tree
import BuilderUtil exposing (..)
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
        , displayedPgBuilderView : BuilderView Uuid
        , displayedPgId : Maybe Uuid
        , environments : List Environment
        , selectedEnvironmentToRunId : Maybe Uuid
        , page : Page
        , runnerRunning : Bool
        , navigationKey : Navigation.Key
        , pgNewNode : NewNode
    }


-- * message


type Msg
    = BuilderMsg PgBuilder.Msg
    | TreeMsg Tree.Msg
    | SelectEnvironment Uuid


-- * update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        SelectEnvironment id ->
            let
                newModel =
                    { model | selectedEnvironmentToRunId = Just id }
            in
            ( newModel, Cmd.none )

        TreeMsg subMsg ->
            let
                newModel =
                    Tree.update subMsg model
            in
            ( newModel, Cmd.none )

        BuilderMsg subMsg ->
            let
                ( newModel, newSubMsg ) =
                    PgBuilder.update subMsg model
            in
            ( newModel, Cmd.map BuilderMsg newSubMsg )


-- * view


view : Model a -> Element Msg
view model =
    column [ width fill, spacing 10 ]
        [ if not model.runnerRunning then enableRunnerBanner else none
        , wrappedRow
              [ width fill
              , paddingXY 10 0
              , spacing 20
              ]
              [ column [ spacing 20, alignTop ]
                    [ el ( box [ alignTop
                               , spacing 20
                               , centerX
                               , padding 20
                               , width (fillPortion 1)
                               ]
                         ) <| environmentSelectionView model.environments model.selectedEnvironmentToRunId SelectEnvironment
                    , el ( box [ alignTop
                               , spacing 20
                               , centerX
                               , padding 20
                               , width (fillPortion 1)
                               ]
                         ) <| map TreeMsg (Tree.view model)
                    ]
              , el [ width (fillPortion 9), height fill, centerX, alignTop ] <|
                  map BuilderMsg (PgBuilder.view model)
              ]
        ]
