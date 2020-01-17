module Application.App exposing (..)

import Http as Http
import Api.Generated as Client
import Api.Converter as Client
import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import ViewUtil exposing (..)
import Html as Html

import InitializedApplication.Model as InitializedApplication
import InitializedApplication.App as InitializedApplication
import BuilderApp.Model as BuilderApp
import Application.Type exposing (..)
import Url as Url
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Navigation
import Tuple as Tuple
import Page exposing (..)
import Url.Parser as Url exposing ((</>))


-- ** model


type alias Model =
    { page : Page
    , navigationKey : Navigation.Key
    , appState : AppState
    }

type AppState
    = SessionPending
    | AppDataPending
      { session : Session
      , mRequestCollection : Maybe BuilderApp.RequestCollection
      , mEnvironments : Maybe (List Environment)
      }
    | InitializedApp InitializedApplication.Model


-- ** message


type Msg
    = SessionFetched Session
    | LinkClicked UrlRequest
    | UrlChanged Url.Url
    | RequestCollectionFetched BuilderApp.RequestCollection
    | EnvironmentsFetched (List Environment)
    | InitializedApplicationMsg InitializedApplication.Msg
    | ServerError Http.Error


-- ** init


init : () -> Url.Url -> Navigation.Key -> (Model, Cmd Msg)
init _ url navKey =
    let
        msg =
            Client.getApiSessionWhoami "" "" getSessionWhoamiResult

        page =
            urlToPage url

        appState =
            SessionPending

        model =
            { page = page
            , navigationKey = navKey
            , appState = appState
            }

    in
        (model, msg)

getSessionWhoamiResult : Result Http.Error Client.Session -> Msg
getSessionWhoamiResult result =
    case result of
        Ok session ->
            let
                newSession =
                    Client.convertSessionFromBackToFront session
            in
                SessionFetched newSession

        Err error ->
            Debug.log "whoami" (ServerError error)


-- ** update


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
                    Client.getApiRequestCollectionByRequestCollectionId "" (getCsrfToken session) (getSessionId session) requestCollectionResultToMsg

                getEnvironments =
                    Client.getApiEnvironment "" (getCsrfToken session) environmentsResultToMsg

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
                            |> upgradeModel

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
                            |> upgradeModel

                _ ->
                    Debug.todo "already initialized app received initialization infos"

        InitializedApplicationMsg subMsg ->
            case model.appState of
                InitializedApp initializedApplication ->
                    let
                        (newInitializedApplication, newMsg) =
                            InitializedApplication.update subMsg initializedApplication

                        newModel =
                            { model | appState = InitializedApp newInitializedApplication }

                    in
                        (newModel, Cmd.map InitializedApplicationMsg newMsg)

                _ ->
                    Debug.todo "InitializedApplicationMsg received with unitialized Application - This should never happen"


        ServerError error ->
            Debug.todo "server error" error

        UrlChanged url ->
            let
                newPage =
                    urlToPage url

                newModel =
                    { model | page = newPage }

            in
                (newModel, Cmd.none)

        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    (model, Navigation.pushUrl model.navigationKey <| Url.toString url)

                External url ->
                    (model, Navigation.load url)

-- ** util


requestCollectionResultToMsg : Result Http.Error Client.RequestCollection -> Msg
requestCollectionResultToMsg result =
    case result of
        Ok requestCollection ->
            let
                newRequestCollection =
                    Client.convertRequestCollectionFromBackToFront requestCollection
            in
                RequestCollectionFetched newRequestCollection

        Err error ->
            ServerError error

environmentsResultToMsg : Result Http.Error (List Client.Environment) -> Msg
environmentsResultToMsg result =
    case result of
        Ok clientEnvironments ->
            let
                environments = List.map Client.convertEnvironmentFromBackToFront clientEnvironments
            in
                EnvironmentsFetched environments

        Err error ->
            ServerError error

upgradeModel : Model -> (Model, Cmd Msg)
upgradeModel model =
    case model.appState of
        AppDataPending { session, mRequestCollection, mEnvironments } ->
            case (mRequestCollection, mEnvironments) of
                (Just requestCollection, Just environments) ->
                    let
                        newAppState =
                            InitializedApp <|
                                InitializedApplication.createModel model.page session requestCollection environments

                        newModel =
                            { model | appState = newAppState }
                    in
                        (newModel, Cmd.none)

                _ ->
                    (model, Cmd.none)

        _ ->
            (model, Cmd.none)


-- ** view


view : Model -> Browser.Document Msg
view model =
    let
        body =
            layout [] <|
                case model.appState of
                    SessionPending ->
                        loadingView

                    AppDataPending _ ->
                        loadingView

                    InitializedApp initializedApplication ->
                        el [ width fill ] <|
                            map InitializedApplicationMsg (InitializedApplication.view initializedApplication)
    in
        { title = "test"
        , body = [body]
        }


unsignedView : Element Msg
unsignedView = none

loadingView : Element a
loadingView =
    el [ width fill
       , height fill
       , Background.color <| secondaryColor
       ]
    <| el [ centerX
          , centerY
          , Font.center
          ]
        <| iconWithText "autorenew" "loading ApiTester..."



-- ** subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
