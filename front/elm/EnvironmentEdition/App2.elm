module EnvironmentEdition.App2 exposing (..)

import Api.Converter as Client
import Api.WebGeneratedClient as Client
import Application.Type exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Page exposing (..)
import Element.Input as Input
import Http
import List.Extra as List
import Util exposing (..)
import StringTemplate exposing(..)
import Browser.Navigation as Navigation
import HttpError exposing (..)
import Uuid exposing (Uuid)
import EnvironmentEdition.Tree.App as Tree
import EnvironmentEdition.Builder.App as Builder

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
        [ wrappedRow
              [ width fill
              , paddingXY 10 0
              , spacing 10
              ]
              [ column
                    ( box [ alignTop
                        , spacing 20
                        , centerX
                        , padding 20
                        , width (fillPortion 1)
                        ]
                    )
                    [ el [ paddingXY 10 0 ] (map TreeMsg (Tree.view model))
                    ]
              , el [ width (fillPortion 9), height fill, centerX, alignTop ] <|
                  map BuilderMsg (Builder.view model)
              ]
        ]
