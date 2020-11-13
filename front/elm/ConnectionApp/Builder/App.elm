module ConnectionApp.Builder.App exposing (..)

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
import Html
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
import ConnectionApp.Builder.Landing.App as Landing
import ConnectionApp.Builder.Edit.App as Edit
import ConnectionApp.Builder.Run.App as Run


-- * model


type alias Model a =
    { a
        | pgConnections : List PgConnection
        , selectedPgConnectionId : Maybe Int
        , selectedPgConnectionToRun : Maybe Int
        , displayedPgConnectionMenuId : Maybe Int
        , displayedConnectionBuilderView : BuilderView Int
        , newConnectionName : String
        , notification : Maybe Notification
        , page : Page
        , navigationKey : Navigation.Key
    }


-- * message


type Msg
    = LandingAppMsg Landing.Msg
    | EditAppMsg Edit.Msg
    | RunAppMsg Run.Msg


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

        EditAppMsg subMsg ->
            let
                (newModel, newMsg) =
                    Edit.update subMsg model
            in
            (newModel, Cmd.map EditAppMsg newMsg)

        RunAppMsg subMsg ->
            let
                (newModel, newMsg) =
                    case getBuilder model of
                        RunView (Just connection) ->
                            let
                                (updatedModel, newConnection, updatedMsg) =
                                    Run.update subMsg model connection

                                mNewConnections =
                                    List.updateIf (\elem -> elem.id == newConnection.id) (always newConnection) model.pgConnections

                            in
                            ( { updatedModel | pgConnections = mNewConnections }
                            , Cmd.map RunAppMsg updatedMsg
                            )

                        _ ->
                            (model, Cmd.none)
            in
            ( newModel, newMsg )


-- * util


getBuilder : Model a -> BuilderView (Maybe PgConnection)
getBuilder model =
    let
        findEnv : Int -> Maybe PgConnection
        findEnv id =
            List.find (\env -> env.id == id) model.pgConnections
    in
    case model.displayedConnectionBuilderView of
        LandingView whichDefaultView ->
            LandingView whichDefaultView

        EditView whichEditView ->
            EditView (mapEditView findEnv whichEditView)

        RunView id ->
            RunView (findEnv id)


-- * view


view : Model a -> Element Msg
view model =
    case getBuilder model of
        LandingView whichView ->
            map LandingAppMsg (Landing.view whichView model)

        EditView whichEditView ->
            case (traverseEditViewMaybe whichEditView) of
                Nothing ->
                    text "404 - could not find edit view"

                Just environment ->
                    map EditAppMsg (Edit.view environment model)

        RunView (Just environment) ->
            map RunAppMsg (Run.view model environment)

        RunView _ ->
            text "404 - could not find run view"
