module ConnectionApp.Builder.Landing.App exposing (..)

import Api.Converter as Client
import Random
import Api.WebGeneratedClient as Client exposing (Id(..))
import Api.RunnerGeneratedClient as Client
import Application.Type exposing (..)
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
                    { model | newConnectionName = newName }
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
                    { newConnectionId = Id newId
                    , newConnectionName = newName
                    }

                newMsg =
                    Cmd.none
                    --Client.postApiConnection "" (getCsrfToken model.session) payload (newConnectionResultToMsg newName newId)
            in
            ( model, newMsg )

        Touch newName id ->
            let
                newConnection =
                    { name = NotEdited newName
                    , id = 0
                    , dbHost = NotEdited ""
                    , dbPassword = NotEdited ""
                    , dbPort  = NotEdited ""
                    , dbUser  = NotEdited ""
                    , dbName  = NotEdited ""
                    }

                newConnections =
                    model.pgConnections ++ [ newConnection ]

                newModel =
                    { model | pgConnections = newConnections }
            in
            ( newModel, Cmd.none )

        PrintNotification notification ->
            ( { model | notification = Just notification }, Cmd.none )


-- * util


newConnectionResultToMsg : String -> Uuid -> Result Http.Error () -> Msg
newConnectionResultToMsg newName id result =
    case result of
        Ok () ->
            Touch newName id

        Err err ->
            PrintNotification <| AlertNotification "Could not create a new connection, try reloading the page!" (httpErrorToString err)


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
                                       | title = " Create a new Connection"
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
            case String.isEmpty model.newConnectionName of
                True -> none
                False ->
                    Input.button primaryButtonAttrs
                        { onPress = Just (GenerateRandomUUIDForFile model.newConnectionName)
                        , label =
                            iconWithAttr { defaultIconAttribute
                                             | title = "Create connection"
                                             , icon = "note_add"
                                         }
                        }

        nameInput =
            Input.text []
                  { onChange = ChangeName
                  , text = model.newConnectionName
                  , placeholder = Just <| Input.placeholder [] (text "my connection")
                  , label = Input.labelLeft [ centerY ] <| text "Connection name: "
                  }
        title =
            el [ Font.size 25, Font.underline ] (text "Create new Connection")
    in
    column [ spacing 20 ]
        [ row [ width fill, centerY ]
              [ el [ alignLeft ] title
              , el [ alignRight ] (closeBuilderView model.page)
              ]
        , nameInput
        , el [ centerX ] createButton
        ]
