port module AppLoader exposing (..)

import Animation
import Animation.Messenger as Messenger
import Api.Converter as Client
import Api.WebGeneratedClient as Client
import Application.Type exposing (..)
import Browser
import Browser.Navigation as Navigation
import Element exposing (..)
import Element.Background as Background
import Http
import Json.Encode as E
import Url as Url
import Url.Parser as Url
import Url.Parser.Query as Query
import Util exposing (..)



{- This is the first app that will get load.
   It will display a loader screen while fetching the necessary data:
   - user session
   - user data

   Once retrieved, it will send those data through a port to the real application
-}
-- * main


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        , view = view
        }


port loadApplication : E.Value -> Cmd msg



-- * model


-- ** loader model


type alias Model =
    { appState : LoaderState
    , page : Page
    , loaderStyle : Animation.State
    , backgroundStyle : Messenger.State Msg
    , navigationKey : Navigation.Key
    }


type LoaderState
    = SessionPending -- first state: we wait for the backend to give us a session
    | AppDataPending
        -- second state: we wait for the backend to give us the session's related data
        { session : Client.Session
        , mRequestCollection : Maybe Client.RequestCollection
        , mPgCollection : Maybe Client.PgCollection
        , mScenarioCollection : Maybe Client.ScenarioCollection
        , mEnvironments : Maybe (List Client.Environment)
        }
    | DataLoaded -- third state: we can fade out the loader
    | StopLoader -- fourth state: we can hide the loader



-- * page


-- ** model


type Page
    = LoadingPage
    | OAuthCallbackPage String



-- ** parser


urlToPage : Url.Url -> Page
urlToPage url =
    let
        {-
           when dealing with github oauth, the callback url cannot contains '#'
           so we instead returns the root url with only a 'code' query param
           eg: host.com?code=someCode
        -}
        parseOAuth : Maybe Page
        parseOAuth =
            case Url.parse (Url.query (Query.string "code")) url of
                Just (Just code) ->
                    Just (OAuthCallbackPage code)

                _ ->
                    Nothing
    in
    case parseOAuth of
        Just oauthPage ->
            oauthPage

        Nothing ->
            LoadingPage


-- * port


-- ** model


type alias LoadedData =
    { session : Client.Session
    , requestCollection : Client.RequestCollection
    , pgCollection : Client.PgCollection
    , scenarioCollection : Client.ScenarioCollection
    , environments : List Client.Environment
    }


-- ** encoder


loadedDataEncoder : LoadedData -> E.Value
loadedDataEncoder { session, requestCollection, pgCollection, environments, scenarioCollection } =
    E.object
        [ ( "session", Client.jsonEncSession session )
        , ( "environments", E.list Client.jsonEncEnvironment environments )
        , ( "requestCollection", Client.jsonEncRequestCollection requestCollection )
        , ( "pgCollection", Client.jsonEncPgCollection pgCollection )
        , ( "scenarioCollection", Client.jsonEncScenarioCollection scenarioCollection )
        ]


-- ** start main app


startMainApp : Model -> ( Model, Cmd Msg )
startMainApp model =
    case model.appState of
        AppDataPending { session, mRequestCollection, mPgCollection, mScenarioCollection, mEnvironments } ->
            let
                dataPending : Maybe { requestCollection : Client.RequestCollection
                                    , pgCollection : Client.PgCollection
                                    , scenarioCollection : Client.ScenarioCollection
                                    , environments : List Client.Environment
                                    }
                dataPending =
                    mRequestCollection
                        |> Maybe.andThen (\requestCollection -> mScenarioCollection
                             |> Maybe.andThen (\scenarioCollection -> mEnvironments
                                 |> Maybe.andThen (\environments -> mPgCollection
                                     |> Maybe.andThen (\pgCollection ->
                                                       Just { requestCollection = requestCollection
                                                            , pgCollection = pgCollection
                                                            , scenarioCollection = scenarioCollection
                                                            , environments = environments
                                                            }
                                                      )
                                                  )
                                              )
                                         )

            in
            case dataPending of
                Just { requestCollection, pgCollection, scenarioCollection, environments } ->
                    let
                        loadedData =
                            { session = session
                            , requestCollection = requestCollection
                            , pgCollection = pgCollection
                            , scenarioCollection = scenarioCollection
                            , environments = environments
                            }

                        newBackgroundStyle =
                            Animation.interrupt
                                [ Animation.to
                                    [ Animation.opacity 0
                                    ]
                                , Messenger.send (LoaderConcealed loadedData)
                                ]
                                model.backgroundStyle

                        newLoaderStyle =
                            Animation.interrupt
                                [ Animation.to [ Animation.rotate (Animation.turn 1) ]
                                ]
                                model.loaderStyle

                        newModel =
                            { model
                                | appState = DataLoaded
                                , backgroundStyle = newBackgroundStyle
                                , loaderStyle = newLoaderStyle
                            }
                    in
                    ( newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- * message


type Msg
    = SessionFetched Client.Session
    | RequestCollectionFetched Client.RequestCollection
    | PgCollectionFetched Client.PgCollection
    | EnvironmentsFetched (List Client.Environment)
    | LoaderConcealed LoadedData
    | ServerError Http.Error
    | Animate Animation.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ScenarioCollectionFetched Client.ScenarioCollection



-- * init


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url navigationKey =
    let
        page =
            urlToPage url

        appState =
            SessionPending

        loaderStyle =
            Animation.interrupt
                [ Animation.loop
                    [ Animation.to [ Animation.rotate (Animation.turn 1) ]
                    , Animation.set [ Animation.rotate (Animation.turn 0) ]
                    ]
                ]
                (Animation.style [])

        backgroundStyle =
            Animation.style [ Animation.opacity 1 ]

        model =
            { appState = appState
            , page = page
            , loaderStyle = loaderStyle
            , backgroundStyle = backgroundStyle
            , navigationKey = navigationKey
            }
    in
    case page of
        OAuthCallbackPage code ->
            ( model, fetchGithubProfile code )

        _ ->
            ( model, Client.getApiSessionWhoami "" "" getSessionWhoamiResult )



-- * update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SessionFetched session ->
            let
                newAppState =
                    AppDataPending
                        { session = session
                        , mRequestCollection = Nothing
                        , mPgCollection = Nothing
                        , mScenarioCollection = Nothing
                        , mEnvironments = Nothing
                        }

                getRequestCollection =
                    Client.getApiRequestCollection "" (getCsrfToken (Client.convertSessionFromBackToFront session)) requestCollectionResultToMsg

                getPgCollection =
                    Client.getApiPgCollection "" (getCsrfToken (Client.convertSessionFromBackToFront session)) pgCollectionResultToMsg

                getScenarioCollection =
                    Client.getApiScenarioCollection "" (getCsrfToken (Client.convertSessionFromBackToFront session)) scenarioCollectionResultToMsg

                getEnvironments =
                    Client.getApiEnvironment "" (getCsrfToken (Client.convertSessionFromBackToFront session)) environmentsResultToMsg

                setBlankUrl =
                    case model.page of
                        OAuthCallbackPage _ ->
                            Navigation.replaceUrl model.navigationKey "/"

                        _ ->
                            Cmd.none

                getAppData =
                    Cmd.batch
                        [ getRequestCollection
                        , getPgCollection
                        , getScenarioCollection
                        , getEnvironments
                        , setBlankUrl
                        ]

                newModel =
                    { model | appState = newAppState }
            in
            ( newModel, getAppData )

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

        PgCollectionFetched pgCollection ->
            case model.appState of
                AppDataPending pending ->
                    let
                        newState =
                            AppDataPending { pending | mPgCollection = Just pgCollection }
                    in
                    { model | appState = newState }
                        |> startMainApp

                _ ->
                    Debug.todo "already initialized app received initialization infos"

        ScenarioCollectionFetched scenarioCollection ->
            case model.appState of
                AppDataPending pending ->
                    let
                        newState =
                            AppDataPending { pending | mScenarioCollection = Just scenarioCollection }
                    in
                    { model | appState = newState }
                        |> startMainApp

                _ ->
                    Debug.todo "already initialized app received initialization infos"

        LoaderConcealed loadedData ->
            let
                newModel =
                    { model | appState = StopLoader }
            in
            ( newModel, loadApplication (loadedDataEncoder loadedData) )

        ServerError error ->
            Debug.todo "server error" error

        UrlChanged _ ->
            ( model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )

        Animate subMsg ->
            let
                ( newBackgroundStyle, cmd ) =
                    Messenger.update subMsg model.backgroundStyle

                newLoaderStyle =
                    Animation.update subMsg model.loaderStyle

                newModel =
                    { model
                        | backgroundStyle = newBackgroundStyle
                        , loaderStyle = newLoaderStyle
                    }
            in
            ( newModel, cmd )



-- * util


fetchGithubProfile : String -> Cmd Msg
fetchGithubProfile code =
    let
        payload =
            Client.SignInWithGithub { signInWithGithubCode = code }

        resultHandler : Result Http.Error Client.Session -> Msg
        resultHandler result =
            case result of
                Ok session ->
                    SessionFetched session

                Err _ ->
                    Debug.todo "todo"
    in
    Client.postApiSessionSignInWithGithub "" payload resultHandler


getSessionWhoamiResult : Result Http.Error Client.Session -> Msg
getSessionWhoamiResult result =
    case result of
        Ok session ->
            SessionFetched session

        Err error ->
            ServerError error


requestCollectionResultToMsg : Result Http.Error Client.RequestCollection -> Msg
requestCollectionResultToMsg result =
    case result of
        Ok requestCollection ->
            RequestCollectionFetched requestCollection

        Err error ->
            ServerError error

pgCollectionResultToMsg : Result Http.Error Client.PgCollection -> Msg
pgCollectionResultToMsg result =
    case result of
        Ok pgCollection ->
            PgCollectionFetched pgCollection

        Err error ->
            ServerError error

scenarioCollectionResultToMsg : Result Http.Error Client.ScenarioCollection -> Msg
scenarioCollectionResultToMsg result =
    case result of
        Ok scenarioCollection ->
            ScenarioCollectionFetched scenarioCollection

        Err error ->
            ServerError error


environmentsResultToMsg : Result Http.Error (List Client.Environment) -> Msg
environmentsResultToMsg result =
    case result of
        Ok environments ->
            EnvironmentsFetched environments

        Err error ->
            ServerError error


-- * view


view : Model -> Browser.Document Msg
view model =
    let
        element =
            case model.appState of
                SessionPending ->
                    loadingView model

                AppDataPending _ ->
                    loadingView model

                DataLoaded ->
                    loadingView model

                StopLoader ->
                    none
    in
    { title = "Loading Patchgirl..."
    , body = [ layout [ Background.color lightGrey ] element ]
    }


loadingView : Model -> Element a
loadingView model =
    let
        loaderAttr =
            List.map htmlAttribute (Animation.render model.loaderStyle)

        backgroundAttr =
            List.map htmlAttribute (Animation.render model.backgroundStyle)

        staticAttr =
            [ width fill
            , height fill
            , Background.color <| secondaryColor
            ]
                ++ backgroundAttr
    in
    el staticAttr <|
        el
            [ centerX
            , centerY
            ]
        <|
            el loaderAttr (text "Loading")



-- * subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Animation.subscription Animate [ model.backgroundStyle ]
        , Animation.subscription Animate [ model.loaderStyle ]
        ]
