module ScenarioBuilderApp.App exposing (..)

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
import Modal exposing (Modal(..))
import Page exposing (..)
import ScenarioBuilderApp.ScenarioBuilder.App as Builder
import ScenarioBuilderApp.ScenarioTree.App as Tree
import Util exposing (..)
import Uuid exposing(Uuid)
import Browser.Navigation as Navigation
import BuilderUtil exposing (..)
import Banner exposing (..)


-- * model


type alias Model a =
    { a
        | notification : Maybe Notification
        , session : Session
        , whichModal : Maybe Modal
        , pgNewNode : NewNode
        , displayedPgBuilderView : BuilderView Uuid
        , requestCollection : RequestCollection
        , pgCollection : PgCollection
        , scenarioCollection : ScenarioCollection
        , displayedScenarioNodeMenuId : Maybe Uuid
        , displayedScenarioBuilderView : RichBuilderView Uuid SceneDetailView
        , environments : List Environment
        , page : Page
        , scenarioNewNode : NewNode
        , selectedEnvironmentToRunId : Maybe Uuid
        , runnerRunning : Bool
        , navigationKey : Navigation.Key
    }



-- * message


type Msg
    = BuilderMsg Builder.Msg
    | TreeMsg Tree.Msg



-- * update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        BuilderMsg subMsg ->
            let
                ( newModel, newSubMsg ) =
                    Builder.update subMsg model
            in
            ( newModel, Cmd.map BuilderMsg newSubMsg )

        TreeMsg subMsg->
            (Tree.update subMsg model, Cmd.none)


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
              [ el ( box [ alignTop
                               , spacing 20
                               , centerX
                               , padding 20
                               , width (fillPortion 1)
                               ]
                         ) <| map TreeMsg (Tree.view model)
              , el [ width (fillPortion 9), height fill, centerX, alignTop ] <|
                  map BuilderMsg (Builder.view model)
              ]
        ]
