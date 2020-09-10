module EnvironmentEdition.Tree.App exposing (..)

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
        | environments : List Environment
        , displayedEnvId : Maybe Uuid
        , displayedEnvironmentBuilderView : BuilderView Uuid
        , displayedEnvironmentNodeMenuId : Maybe Uuid
        , navigationKey : Navigation.Key
    }


-- * message


type Msg
    = ToggleMenu (Maybe Uuid)


-- * update


update : Msg -> Model a -> Model a
update msg model =
    case msg of
        ToggleMenu mId ->
            { model | displayedEnvironmentNodeMenuId = mId }


-- * view


view : Model a -> Element Msg
view model =
    column [ spacing 10 ] <|
        List.map (nodeView model) model.environments


nodeView : Model a -> Environment -> Element Msg
nodeView model environment =
    let
        selected =
             getBuilderId model.displayedEnvironmentBuilderView == Just environment.id

        showMenu =
            model.displayedEnvironmentNodeMenuId == Just environment.id

        menuView : Element Msg
        menuView =
            case showMenu of
                True ->
                    link []
                        { url = href (EnvPage2 (EditView (DefaultEditView environment.id)))
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
            el [ onMouseEnter (ToggleMenu (Just environment.id))
               , onMouseLeave (ToggleMenu Nothing)
               , weight
               , onRight menuView
               ] <|
                link []
                    { url = href (EnvPage2 (RunView environment.id))
                    , label = el [] <| iconWithTextAndColor "label" (notEditedValue environment.name) color
                    }
    in
    linkView
