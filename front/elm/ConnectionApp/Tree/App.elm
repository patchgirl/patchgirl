module ConnectionApp.Tree.App exposing (..)

import Api.WebGeneratedClient as Client
import Application.Type exposing (..)
import Element exposing (..)
import Element.Input as Input
import Element.Font as Font
import Http
import Page exposing (..)
import Random
import Util exposing (..)
import Uuid exposing (Uuid)
import Browser.Navigation as Navigation
import HttpError exposing(..)
import Element.Events exposing (..)
import BuilderUtil exposing (..)


-- * model


type alias Model a =
    { a
        | pgConnections : List PgConnection
        , selectedPgConnectionId : Maybe Int
        , displayedPgConnectionMenuId : Maybe Int
        , notification : Maybe Notification
        , page : Page
        , navigationKey : Navigation.Key
    }


-- * message


type Msg
    = ToggleMenu (Maybe Int)


-- * update


update : Msg -> Model a -> Model a
update msg model =
    case msg of
        ToggleMenu mId ->
            { model | displayedPgConnectionMenuId = mId }


-- * view


view : Model a -> Element Msg
view model =
    column [ spacing 10 ] <|
        List.map (nodeView model) model.pgConnections


nodeView : Model a -> PgConnection -> Element Msg
nodeView model pgConnection =
    let
        selected =
             model.selectedPgConnectionId == Just pgConnection.id

        showMenu =
            model.displayedPgConnectionMenuId == Nothing

        menuView : Element Msg
        menuView =
            case showMenu of
                True ->
                    link []
                        { url = href (ConnectionPage (EditView (DefaultEditView pgConnection.id)))
                        , label = editIcon
                        }

                False ->
                    none

        color =
            case selected of
                True -> primaryColor
                False -> secondaryColor

        weight =
            case selected of
               True -> Font.heavy
               False -> Font.regular

        linkView =
            el [ onMouseEnter (ToggleMenu (Just pgConnection.id))
               , onMouseLeave (ToggleMenu Nothing)
               , weight
               , onRight menuView
               ] <|
                link []
                    { url = href (ConnectionPage (RunView pgConnection.id))
                    , label = el [] <| iconWithTextAndColor "label" (notEditedValue pgConnection.name) color
                    }
    in
    linkView
