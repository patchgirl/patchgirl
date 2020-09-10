module EnvironmentEdition.Builder.App exposing (..)

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
import EnvironmentEdition.Builder.Landing.App as Landing
--import EnvironmentEdition.Builder.Edit.App as Edit
--import EnvironmentEdition.Builder.Run.App as Run


-- * model


type alias Model a =
    { a
        | environments : List Environment
        , newEnvironmentName : String
        , session : Session
        , notification : Maybe Notification
        , displayedEnvId : Maybe Uuid
        , displayedEnvironmentNodeMenuId : Maybe Uuid
        , displayedEnvironmentBuilderView : BuilderView Uuid
        , navigationKey : Navigation.Key
        , page : Page
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
{-
        EditAppMsg subMsg ->
            let
                (newModel, newMsg) =
                    Edit.update subMsg model
            in
            (newModel, Cmd.map EditAppMsg newMsg)

        RunAppMsg subMsg ->
            let
                (updatedModel, newPgRecord, newMsg) =
                    case getBuilder model of
                        RunView (Just (File pgFileRecord)) ->
                            Run.update subMsg model pgFileRecord
                        _ ->
                            Debug.todo ""

                (PgCollection pgCollectionId pgNodes) =
                    model.pgCollection

                newBuilderTree =
                    List.map (modifyPgNode newPgRecord.id (always (File newPgRecord))) pgNodes

                newModel =
                    { updatedModel
                        | pgCollection = PgCollection pgCollectionId newBuilderTree
                    }

            in
            ( newModel, Cmd.map RunAppMsg newMsg )
-}


-- * util


getBuilder : Model a -> BuilderView (Maybe Environment)
getBuilder model =
    let
        findEnv : Uuid -> Maybe Environment
        findEnv id =
            List.find (\env -> env.id == id) model.environments
    in
    case model.displayedEnvironmentBuilderView of
        LandingView whichDefaultView ->
            LandingView whichDefaultView

        EditView whichEditView ->
            EditView (mapEditView findEnv whichEditView)

        RunView id ->
            Debug.todo ""
            --RunView (findPgNode nodes id)


-- * view


view : Model a -> Element Msg
view model =
    case getBuilder model of
        LandingView whichView ->
            map LandingAppMsg (Landing.view whichView model)

        _ ->
            none
{-
        EditView whichEditView ->
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
