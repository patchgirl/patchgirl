module ScenarioBuilderApp.ScenarioBuilder.App2 exposing (..)

import Animation
import Api.Converter as Client
import Random
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
import Page exposing(..)
import Runner
import HttpError exposing(..)
import Interpolator exposing(..)
import Browser.Navigation as Navigation
import BuilderUtil exposing (..)
import Application.Model as Application
import ScenarioBuilderApp.ScenarioBuilder.Landing.App as Landing
--import SCENARIOBuilderApp.SCENARIOBuilder.Edit.App as Edit
--import SCENARIOBuilderApp.SCENARIOBuilder.Run.App as Run


-- * model


type alias Model a =
    { a
        | notification : Maybe Notification
        , scenarioCollection : ScenarioCollection
        , displayedScenarioNodeMenuId : Maybe Uuid
        , displayedScenarioBuilderView : RichBuilderView Uuid (Maybe Uuid)
        , scenarioNewNode : NewNode
        , environments : List Environment
        , selectedEnvironmentToRunIndex : Maybe Uuid
        , page : Page
        , runnerRunning : Bool
        , navigationKey : Navigation.Key
    }


-- * message


type Msg
    = LandingAppMsg Landing.Msg
--    | EditAppMsg Edit.Msg
--    | RunAppMsg Run.Msg


-- * update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        LandingAppMsg subMsg ->
            let
                (newModel, newMsg) =
                    Landing.update subMsg model
            in
            (newModel, Cmd.map LandingAppMsg newMsg)



-- * util


getBuilder : Model a -> RichBuilderView (Maybe ScenarioNode) (Maybe Uuid)
getBuilder model =
    let
        (ScenarioCollection _ nodes) =
            model.scenarioCollection
    in
    case model.displayedScenarioBuilderView of
        RichLandingView whichDefaultView ->
            RichLandingView whichDefaultView

        RichEditView whichEditView ->
            let
                getChildren folder =
                    let (ScenarioChildren children) = folder.children
                    in children
            in
            RichEditView (mapEditView (findNode nodes getChildren) whichEditView)

        RichRunView id mId ->
            RichRunView (findScenarioNode nodes id) mId


-- * view


view : Model a -> Element Msg
view model =
    case getBuilder model of
        RichLandingView whichView ->
            map LandingAppMsg (Landing.view whichView model)
{-
        RichEditView whichEditView ->
            case (traverseEditViewMaybe whichEditView) of
                Nothing ->
                    text "404 - could not find edit view"

                Just nodeType ->
                    map EditAppMsg (Edit.view nodeType model)

        RunView (Just (File pgFileRecord)) ->
            map RunAppMsg (Run.view model pgFileRecord)

        RunView _ ->
            text "404 - could not find run view"
-}

        _ -> none