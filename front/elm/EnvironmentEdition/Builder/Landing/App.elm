

module EnvironmentEdition.Builder.Landing.App exposing (..)

import Api.Converter as Client
import Random
import Api.WebGeneratedClient as Client
import Api.RunnerGeneratedClient as Client
import Application.Type exposing (..)
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
import Util exposing (..)
import Uuid exposing (Uuid)
import Page exposing(..)
import HttpError exposing(..)
import BuilderUtil as PgTree
import BuilderUtil exposing (..)
import Animation
import Browser.Navigation as Navigation


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


-- * msg


type Msg
    = ChangeName String
    -- touch
    | GenerateRandomUUIDForFile String
    | AskTouch String Uuid
    | Touch String Uuid
    -- other
    | PrintNotification Notification


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
        ChangeName newName ->
            let
                newModel =
                    { model | newEnvironmentName = newName }
            in
            (newModel, Cmd.none)

        GenerateRandomUUIDForFile newName ->
            let
                newMsg =
                    Random.generate (AskTouch newName) Uuid.uuidGenerator
            in
            ( model, newMsg )

        AskTouch newName newId ->
            let
                payload =
                    { newEnvironmentId = newId
                    , newEnvironmentName = newName
                    }

                newMsg =
                    Client.postApiEnvironment "" (getCsrfToken model.session) payload (newEnvironmentResultToMsg newName newId)
            in
            ( model, newMsg )

        Touch newName id ->
            let
                newEnvironment =
                    { id = id
                    , name = NotEdited newName
                    , showRenameInput = True
                    , keyValues = []
                    }

                newEnvironments =
                    model.environments ++ [ newEnvironment ]

                newModel =
                    { model | environments = newEnvironments }
            in
            ( newModel, Cmd.none )

        PrintNotification notification ->
            ( { model | notification = Just notification }, Cmd.none )


-- * util


newEnvironmentResultToMsg : String -> Uuid -> Result Http.Error () -> Msg
newEnvironmentResultToMsg newName id result =
    case result of
        Ok () ->
            Touch newName id

        Err err ->
            PrintNotification <| AlertNotification "Could not create a new environment, try reloading the page!" (httpErrorToString err)


-- * view


view : WhichDefaultView -> Model a -> Element Msg
view whichDefaultView model =
    el (box [ Background.color white
            , centerX
            , spacing 20
            , padding 30
            ]
       ) <|
        case whichDefaultView of
            DefaultView ->
                defaultView

            CreateDefaultFolderView ->
                none

            CreateDefaultFileView ->
                createDefaultFileView model


-- ** create default view


defaultView : Element Msg
defaultView =
    let
        newPgLink =
            link primaryButtonAttrs
                { label =
                      iconWithAttr { defaultIconAttribute
                                       | title = " Create a new Environment"
                                       , icon = "note_add"
                                   }
                , url = href (EnvPage (LandingView CreateDefaultFileView))
                }
    in
    el [ centerX ] newPgLink


-- ** create default file view


createDefaultFileView : Model a -> Element Msg
createDefaultFileView model =
    let
        createButton =
            case String.isEmpty model.newEnvironmentName of
                True -> none
                False ->
                    Input.button primaryButtonAttrs
                        { onPress = Just (GenerateRandomUUIDForFile model.newEnvironmentName)
                        , label =
                            iconWithAttr { defaultIconAttribute
                                             | title = "Create environment"
                                             , icon = "note_add"
                                         }
                        }

        nameInput =
            Input.text []
                  { onChange = ChangeName
                  , text = model.newEnvironmentName
                  , placeholder = Just <| Input.placeholder [] (text "my environment")
                  , label = Input.labelLeft [ centerY ] <| text "Environment name: "
                  }
        title =
            el [ Font.size 25, Font.underline ] (text "Create new Environment")
    in
    column [ spacing 20 ]
        [ row [ width fill, centerY ]
              [ el [ alignLeft ] title
              , el [ alignRight ] (closeBuilderView model.page)
              ]
        , nameInput
        , el [ centerX ] createButton
        ]
