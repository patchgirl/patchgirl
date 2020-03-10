port module AppLoader exposing (..)

import Browser
import Json.Encode as E
import Application.App as Application
import Animation
import Application.Type exposing (..)
import Http
import InitializedApplication.Model as InitializedApplication
import InitializedApplication.App as InitializedApplication
import Api.Generated as Client
import Api.Converter as Client
import ViewUtil exposing (..)
import Element.Background as Background
import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import Html
import Html.Attributes as Html


-- * main


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

port loadApplication : E.Value -> Cmd msg


-- * model


-- ** loader model


type alias Model =
    { appState : AppState
    , style : Animation.State
    }

type AppState
    = SessionPending -- first state: we wait for the backend to give us a session
    | AppDataPending -- second state: we wait for the backend to give us all our session's data
      { session : Client.Session
      , mRequestCollection : Maybe Client.RequestCollection
      , mEnvironments : Maybe (List Client.Environment)
      }
    | InitializedApp InitializedApplication.Model -- third state: we can show the app


-- ** port model


type alias LoadedData =
    { session : Client.Session
    , requestCollection : Client.RequestCollection
    , environments : List Client.Environment
    }

loadedDataEncoder : LoadedData -> E.Value
loadedDataEncoder { session, requestCollection, environments } =
    E.object
        [ ("session", Client.jsonEncSession session)
        , ("environments", E.list Client.jsonEncEnvironment environments)
        , ("requestCollection", Client.jsonEncRequestCollection requestCollection)
        ]


-- * message


type Msg
    = SessionFetched Client.Session
    | RequestCollectionFetched Client.RequestCollection
    | EnvironmentsFetched (List Client.Environment)
    | ServerError Http.Error
    | Animate Animation.Msg


-- * init


initialStyle =
    let
        animation =
            Animation.styleWith (Animation.easing { duration = 10000 * 10000
                                                  , ease = \u -> u
                                                  }
                                ) [ ]

        looper =
            Animation.loop
                [ Animation.toWith (Animation.speed { perSecond = 2 })
                      [ Animation.rotate (Animation.turn 1) ]
                , Animation.set [ Animation.rotate (Animation.turn 0) ]
                ]

    in
        Animation.interrupt [ looper ] animation
        --animation

init : () -> (Model, Cmd Msg)
init _ =
    let
        msg =
            Client.getApiSessionWhoami "" "" getSessionWhoamiResult

        appState =
            SessionPending

        model =
            { appState = appState
            , style = initialStyle
            }

    in
        (model, msg)


-- * update


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SessionFetched session ->
            let
                newAppState =
                    AppDataPending { session = session
                                   , mRequestCollection = Nothing
                                   , mEnvironments = Nothing
                                   }

                getRequestCollection =
                    Client.getApiRequestCollection "" (getCsrfToken (Client.convertSessionFromBackToFront session)) requestCollectionResultToMsg

                getEnvironments =
                    Client.getApiEnvironment "" (getCsrfToken (Client.convertSessionFromBackToFront session)) environmentsResultToMsg

                getAppData =
                    Cmd.batch
                        [ getRequestCollection
                        , getEnvironments
                        ]

                newModel =
                    { model | appState = newAppState }

            in
                (newModel, getAppData)

        EnvironmentsFetched environments ->
            case model.appState of
                AppDataPending pending ->
                    let
                        newState =
                            AppDataPending { pending | mEnvironments = Just environments }
                    in
                        { model | appState = newState }
                            |> startMainApp

                _ ->
                    Debug.todo "already initialized app received initialization infos"

        RequestCollectionFetched requestCollection ->
            case model.appState of
                AppDataPending pending ->
                    let
                        newState =
                            AppDataPending { pending | mRequestCollection = Just requestCollection }
                    in
                        { model | appState = newState }
                            |> startMainApp

                _ ->
                    Debug.todo "already initialized app received initialization infos"


        ServerError error ->
            Debug.todo "server error" error

        Animate subMsg ->
            let
                newModel =
                    { model
                        | style = Animation.update subMsg model.style
                    }

            in
                (newModel, Cmd.none)


-- * util


getSessionWhoamiResult : Result Http.Error Client.Session -> Msg
getSessionWhoamiResult result =
    case result of
        Ok session ->
            SessionFetched session

        Err error ->
            Debug.log "whoami" (ServerError error)


requestCollectionResultToMsg : Result Http.Error Client.RequestCollection -> Msg
requestCollectionResultToMsg result =
    case result of
        Ok requestCollection ->
            RequestCollectionFetched requestCollection

        Err error ->
            ServerError error

environmentsResultToMsg : Result Http.Error (List Client.Environment) -> Msg
environmentsResultToMsg result =
    case result of
        Ok environments ->
            EnvironmentsFetched environments

        Err error ->
            ServerError error


-- * port


startMainApp : Model -> (Model, Cmd Msg)
startMainApp model =
    case model.appState of
        AppDataPending { session, mRequestCollection, mEnvironments } ->
            case (mRequestCollection, mEnvironments) of
                (Just requestCollection, Just environments) ->
                    let
                        loadedData =
                            { session = session
                            , requestCollection = requestCollection
                            , environments = environments
                            }
                    in
                        (model, loadApplication (loadedDataEncoder loadedData))

                _ ->
                    (model, Cmd.none)

        _ ->
            (model, Cmd.none)


-- * view


view : Model -> Html.Html Msg
view model =
    let
        element =
            case model.appState of
                SessionPending ->
                    loadingView model

                AppDataPending _ ->
                    loadingView model

                InitializedApp initializedApplication ->
                    none
    in
        layout [ Background.color lightGrey, htmlAttribute(Html.style "height" "100%") ] element

loadingView : Model -> Element a
loadingView model =
    let
        animationAttr =
            List.map htmlAttribute (Animation.render model.style)

        staticAttr =
            [ width fill
            , height fill
            , Background.color <| secondaryColor
            ]
    in
        el staticAttr
            <| el [ centerX
                  , centerY
                  ]
                <| row [] [ el animationAttr (icon "autorenew")
                          , text "loading patchGirl..."
                          ]



-- * subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
