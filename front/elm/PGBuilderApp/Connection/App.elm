module PGBuilderApp.Connection.App exposing (..)

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


-- * model


type alias Model a =
    { a
        | pgCollection : PgCollection
        , pgConnections : List PgConnection
        , selectedPgConnectionToRun : Maybe Int
        , displayedPgConnectionMenuId : Maybe Int
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
    = DoNothing
    | SelectConnection Int
    | ToggleMenu (Maybe Int)

-- * update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        SelectConnection id ->
            let
                newModel =
                    { model | selectedPgConnectionToRun = Just id }
            in
            (newModel, Cmd.none)

        ToggleMenu id ->
            let
                newModel =
                    { model | displayedPgConnectionMenuId = id }
            in
            (newModel, Cmd.none)

        _ ->
            (model, Cmd.none)


-- * view


view : Model a -> Element Msg
view model =
    let
        entryView : PgConnection -> Input.Option Int Msg
        entryView connection =
            let
                showMenu =
                    model.displayedPgConnectionMenuId == Just connection.id

                selected =
                    model.selectedPgConnectionToRun == Just connection.id

                attributes =
                    case selected of
                        False -> []
                        True -> [ Font.bold ]

                withMenu : Element Msg -> Element Msg
                withMenu e =
                    row [] <|
                        case showMenu of
                              False -> [ e ]
                              True ->
                                  [ e
{-                                  , link []
                                      { url = href (PgPage (EditView (DefaultEditView id)))
                                      , label =
                                          iconWithAttr { defaultIconAttribute
                                                           | title = ""
                                                           , icon = "edit"
                                                       }
                                      } -}
                                  ]

            in
            Input.option connection.id <|
                el (attributes ++ [ onMouseEnter (ToggleMenu (Just connection.id))
                                  , onMouseLeave (ToggleMenu Nothing)
                                  ]
                   ) <|
                    withMenu (text (notEditedValue connection.name))

    in
    Input.radio [ padding 20, spacing 10 ]
        { onChange = SelectConnection
        , selected = model.selectedPgConnectionToRun
        , label = Input.labelAbove [] (text "Connection:")
        , options = List.map entryView model.pgConnections
        }
