module RequestBuilderApp.RequestBuilder.App exposing (..)

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
import RequestBuilderApp.RequestBuilder.ResponseView exposing(..)
import Page exposing(..)
import Runner
import HttpError exposing(..)
import Interpolator exposing(..)
import Browser.Navigation as Navigation
import BuilderUtil exposing (..)
import RequestBuilderApp.RequestTree.App as RequestTree
import Application.Model as Application
import RequestBuilderApp.RequestBuilder.Landing.App as Landing
import RequestBuilderApp.RequestBuilder.Edit.App as Edit
import RequestBuilderApp.RequestBuilder.Run.App as Run


-- * model


type alias Model a =
    { a
        | notification : Maybe Notification
        , requestCollection : RequestCollection
        , displayedRequestNodeMenuId : Maybe Uuid
        , displayedRequestBuilderView : BuilderView Uuid
        , requestNewNode : NewNode
        , environments : List Environment
        , selectedEnvironmentToRunId : Maybe Uuid
        , page : Page
        , runnerRunning : Bool
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
                        RunView (Just (File requestFileRecord)) ->
                            let
                                (updatedModel, newRequestRecord, updatedMsg) =
                                    Run.update subMsg model requestFileRecord

                                (RequestCollection requestCollectionId requestNodes) =
                                    model.requestCollection

                                newBuilderTree =
                                    List.map (modifyNode newRequestRecord.id (always (File newRequestRecord))) requestNodes
                            in
                            ( { updatedModel | requestCollection = RequestCollection requestCollectionId newBuilderTree }
                            , Cmd.map RunAppMsg updatedMsg
                            )

                        _ ->
                            ( model, Cmd.none )
            in
            ( newModel, newMsg )


-- * util


getBuilder : Model a -> BuilderView (Maybe RequestNode)
getBuilder model =
    let
        (RequestCollection _ requestNodes) =
            model.requestCollection
    in
    case model.displayedRequestBuilderView of
        LandingView whichDefaultView ->
            LandingView whichDefaultView

        EditView whichEditView ->
            EditView (mapEditView (findNode requestNodes) whichEditView)

        RunView id ->
            RunView (findNode requestNodes id)


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

                Just nodeType ->
                    map EditAppMsg (Edit.view nodeType model)

        RunView (Just (File requestFileRecord)) ->
            map RunAppMsg (Run.view requestFileRecord model)

        RunView _ ->
            text "404 - could not find run view"
