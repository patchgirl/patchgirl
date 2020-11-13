module ConnectionApp.App exposing (..)

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
import BuilderUtil exposing (..)
import Util exposing (..)
import Uuid exposing (Uuid)
import Api.RunnerGeneratedClient as Client
import Browser.Navigation as Navigation
import Banner exposing (..)
import Element.Input as Input
import Element.Events exposing (..)
import ConnectionApp.Tree.App as Tree
import ConnectionApp.Builder.App as Builder


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
    = TreeMsg Tree.Msg
    | BuilderMsg Builder.Msg


-- * update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        TreeMsg subMsg ->
            (Tree.update subMsg model, Cmd.none)

        BuilderMsg subMsg ->
            let
                ( newModel, newSubMsg ) =
                    Builder.update subMsg model
            in
            ( newModel, Cmd.map BuilderMsg newSubMsg )


-- * view


view : Model a -> Element Msg
view model =
    column [ width fill, height fill, spacing 10 ]
        [ wrappedRow [ width fill
                     , height fill
                     , paddingXY 10 0
                     , spacing 10
                     ]
              [ el (box [ spacing 20, padding 20, height fill ]) <| map TreeMsg (Tree.view model)
              , el [ width (fillPortion 9), height fill, centerX, alignTop ] <|
                  text "coucuo"
              ]
        ]
