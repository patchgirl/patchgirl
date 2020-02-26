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
import SignIn.App as SignIn
import MainNavBar.App as MainNavBar
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
    , url : Url.Url
    }

type AppState
    = SessionPending
    | AppDataPending
      { session : Session
      , mRequestCollection : Maybe RequestCollection
      , mEnvironments : Maybe (List Environment)
      }
    | InitializedApp InitializedApplication.Model


-- ** message


type Msg
    = SessionFetched Session
    | LinkClicked UrlRequest
    | UrlChanged Url.Url
    | RequestCollectionFetched RequestCollection
    | EnvironmentsFetched (List Environment)
    | InitializedApplicationMsg InitializedApplication.Msg
    | ServerError Http.Error


-- ** init


reload : Url.Url -> Navigation.Key -> (Model, Cmd Msg)
reload url navKey =
    let
        msg =
            Cmd.batch [ Client.getApiSessionWhoami "" "" getSessionWhoamiResult
                      , Navigation.pushUrl model.navigationKey <| Debug.log "url" (Url.toString url)
                      ]

        page =
            urlToPage url

        appState =
            SessionPending

        model =
            { page = page
            , navigationKey = navKey
            , appState = appState
            , url = url
            }

    in
        (model, msg)



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
            , url = url
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
                    Client.getApiRequestCollection "" (getCsrfToken session) requestCollectionResultToMsg

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
            case subMsg of
                InitializedApplication.SignInMsg (SignIn.SignInSucceed _) ->
                    resetApp model

                InitializedApplication.MainNavBarMsg (MainNavBar.SignOutSucceed _) ->
                    resetApp model

                _ ->
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
                    changePage model newPage
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


changePage : Model -> Page -> Model
changePage model newPage =
    let
        newModel =
            case model.appState of
                InitializedApp initializedApplicationModel ->
                    let
                        newAppState =
                            InitializedApp { initializedApplicationModel
                                               | page = newPage
                                           }
                    in
                        { model | appState = newAppState }

                _ ->
                    model
    in
        { newModel | page = newPage }


resetApp : Model -> (Model, Cmd Msg)
resetApp model =
    let
        url =
            model.url

        newUrl =
            { url
                | path = "/"
                , query = Nothing
                , fragment = Nothing
            }
    in
        reload newUrl model.navigationKey


-- ** view


view : Model -> Browser.Document Msg
view model =
    let
        body =
            layout [ Background.color lightGrey ] <|
                case model.appState of
                    SessionPending ->
                        loadingView

                    AppDataPending _ ->
                        loadingView

                    InitializedApp initializedApplication ->
                        el [ width fill ] <|
                            map InitializedApplicationMsg (InitializedApplication.view initializedApplication)
    in
        { title = "PatchGirl"
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
        <| iconWithText "autorenew" "loading patchGirl..."



-- ** subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
