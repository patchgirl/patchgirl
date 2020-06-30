module Application.App exposing (..)

import Animation
import Application.Model exposing (..)
import Application.Type exposing (..)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Navigation
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import EnvironmentEdition.App as EnvironmentEdition
import MainNavBar.App as MainNavBar
import Modal exposing (Modal(..))
import Page exposing (..)
import RequestBuilderApp.App as RequestBuilderApp
import RequestBuilderApp.RequestBuilder.App as RequestBuilder
import ScenarioBuilderApp.App as ScenarioBuilderApp
import DocumentationApp.App as DocumentationApp
import TangoScriptApp.App as TangoScriptApp
import ScenarioBuilderApp.ScenarioBuilder.App as ScenarioBuilder
import Url as Url
import Url.Parser as Url
import Util exposing (..)
import Time
import Http
import Api.RunnerGeneratedClient as Client
import Const


-- * model


type alias UserData =
    { session : Session
    , requestCollection : RequestCollection
    , scenarioCollection : ScenarioCollection
    , environments : List Environment
    }


-- * message


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url.Url
    | BuilderAppMsg RequestBuilderApp.Msg
    | EnvironmentEditionMsg EnvironmentEdition.Msg
    | ScenarioMsg ScenarioBuilderApp.Msg
    | TangoScriptMsg TangoScriptApp.Msg
    | MainNavBarMsg MainNavBar.Msg
    | Animate Animation.Msg
    | CheckRunnerStatus
    | RunnerNotRunning
    | RunnerRunning



-- * init


init : UserData -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init { session, requestCollection, environments, scenarioCollection } url navigationKey =
    let
        page =
            urlToPage url

        selectedEnvironmentToEditId =
            Just 0

        selectedEnvironmentToRunIndex =
            Just 0

        initialLoadingStyle =
            Animation.style [ Animation.opacity 0 ]

        loadingAnimation =
            Animation.interrupt
                [ Animation.to
                    [ Animation.opacity 1
                    ]
                ]
                initialLoadingStyle

        initialNotificationAnimation =
            Animation.style [ Animation.opacity 0 ]

        notificationAnimation =
            Animation.interrupt
                [ Animation.to
                    [ Animation.opacity 1
                    ]
                ]
                initialNotificationAnimation

        model =
            { session = session
            , page = page
            , url = url
            , navigationKey = navigationKey
            , loadingAnimation = loadingAnimation
            , notification = Nothing
            , notificationAnimation = notificationAnimation
            , whichModal = Nothing
            , showMainMenuName = Nothing
            , requestCollection = requestCollection
            , displayedRequestNodeMenuId = Nothing
            , scenarioCollection = scenarioCollection
            , displayedScenarioNodeMenuId = Nothing
            , script = ""
            , selectedEnvironmentToRunIndex = selectedEnvironmentToRunIndex
            , selectedEnvironmentToEditId = selectedEnvironmentToEditId
            , environments = environments
            , runnerRunning = False
            }
    in
    ( model, Cmd.none )



-- * update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChanged url ->
            let
                newPage =
                    urlToPage url

                newModel =
                    { model | page = newPage }
            in
            ( newModel, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Navigation.pushUrl model.navigationKey <| Url.toString url )

                External url ->
                    ( model, Navigation.load url )

        BuilderAppMsg subMsg ->
            let
                ( newModel, newMsg ) =
                    RequestBuilderApp.update subMsg model
            in
            ( newModel, Cmd.map BuilderAppMsg newMsg )

        EnvironmentEditionMsg subMsg ->
            case EnvironmentEdition.update subMsg model of
                ( newModel, newSubMsg ) ->
                    ( newModel, Cmd.map EnvironmentEditionMsg newSubMsg )

        ScenarioMsg subMsg ->
            case ScenarioBuilderApp.update subMsg model of
                ( newModel, newSubMsg ) ->
                    ( newModel, Cmd.map ScenarioMsg newSubMsg )

        TangoScriptMsg subMsg ->
            case TangoScriptApp.update subMsg model of
                ( newModel, newSubMsg ) ->
                    ( newModel, Cmd.map TangoScriptMsg newSubMsg )

        MainNavBarMsg subMsg ->
            case MainNavBar.update subMsg model of
                ( newModel, newSubMsg ) ->
                    ( newModel, Cmd.map MainNavBarMsg newSubMsg )

        Animate subMsg ->
            let
                newModel =
                    { model
                        | loadingAnimation =
                            Animation.update subMsg model.loadingAnimation
                        , notificationAnimation =
                            Animation.update subMsg model.notificationAnimation
                    }
            in
            ( newModel, Cmd.none )

        CheckRunnerStatus ->
            let
                newMsg =
                    fetchRunnerStatus
            in
            (model, newMsg)

        RunnerNotRunning ->
            let
                newModel =
                    { model | runnerRunning = False }
            in
            (newModel, Cmd.none)

        RunnerRunning ->
            let
                newModel =
                    { model | runnerRunning = True }
            in
            (newModel, Cmd.none)


-- * util


fetchRunnerStatus : Cmd Msg
fetchRunnerStatus =
    let
        resultHandler : Result Http.Error () -> Msg
        resultHandler result =
            case result of
                Ok () ->
                    RunnerRunning

                Err _ ->
                    RunnerNotRunning
    in
    Client.getApiRunnerHealth Const.runnerUrl resultHandler


-- * view


view : Model -> Browser.Document Msg
view model =
    let
        loadingAnimation =
            List.map htmlAttribute (Animation.render model.loadingAnimation)

        bodyAttr =
            (Background.color lightGrey)
                :: loadingAnimation
                ++ [ inFront (modalView model)
                   , inFront (notificationView model)
                   ]

        body =
            layout bodyAttr (mainView model)
    in
    { title = "PatchGirl"
    , body = [ body ]
    }



-- ** main view


mainView : Model -> Element Msg
mainView model =
    let
        builderView mScenarioId =
            map BuilderAppMsg (RequestBuilderApp.view model mScenarioId)

        appLayout : Element Msg -> Element Msg
        appLayout appView =
            column [ width fill
                   , height fill
                   , centerY
                   , spacing 30
                   ] [ map MainNavBarMsg (MainNavBar.view model)
                     , el [ width fill ] appView
                     ]

    in
        case model.page of
            HomePage ->
                homeView

            NotFoundPage ->
                appLayout <| el [ centerY, centerX ] (text "not found")

            ReqPage _ mFromScenarioId ->
                appLayout <| builderView mFromScenarioId

            EnvPage ->
                appLayout <| map EnvironmentEditionMsg (EnvironmentEdition.view model)

            ScenarioPage _ ->
                appLayout <| map ScenarioMsg (ScenarioBuilderApp.view model)

            DocumentationPage mDocumentation ->
                appLayout (DocumentationApp.view mDocumentation)

            TangoScriptPage ->
                appLayout <| map TangoScriptMsg (TangoScriptApp.view model)


-- ** home view


homeView : Element Msg
homeView =
    let
        banner : Element Msg
        banner =
            row [ height (px 200), width fill
                , Background.color primaryColor
                , paddingXY 0 50
                ]
            [ column [ centerX
                     , spacing 20
                     , Font.color secondaryColor
                     , width fill
                     ]
                  [ image [ centerX, height (px 70) ]
                        { src = "public/images/logo.png"
                        , description = "logo"
                        }
                  , paragraph [ centerX, centerY, Font.size 25, Font.center ] [ text "An open source rest-client to play scenarios of http requests" ]
                  ]
            ]

        feature : String -> String -> String -> String -> Element Msg
        feature title subtitle imageSrc imageDescription =
            column [ spacing 30, alignTop, width fill ]
                [ column [ centerX, spacing 10 ]
                      [ el [ Font.size 20, Font.bold, width fill ] (paragraph [] [ text title ])
                      , el [ Font.size 18, width fill ] (paragraph [] [ text subtitle ])
                      ]
                , image [ centerX ]
                    { src = imageSrc
                    , description = imageDescription
                    }
                ]

        features : Element Msg
        features =
            wrappedRow [ width fill, spacing 0, Font.center, centerX ]
                [ feature
                      "Http request"
                      "Create, share and run http requests from your browser"
                      "/public/images/http_request.png"
                      "create http request"
                , feature
                      "Scenario"
                      "Run and test scenario of http requests"
                      "/public/images/scenario.png"
                      "play scenario of http requests"
                , feature
                      "Free & Open source"
                      "PatchGirl is and will remain open source ❤️"
                      "/public/images/laptop.png"
                      "free and open source"
                ]

        tryIt : Element Msg
        tryIt =
            row [ height (px 100), width fill
                , Background.color primaryColor
                , Font.color secondaryColor, Font.size 25, Font.center
                , spacing 20
                ]
                [ link [ centerY, centerX, Font.underline ]
                      { url = "#app/scenario"
                      , label = text "Try it"
                      }
                ]

        footer : Element Msg
        footer =
            row [ height (px 100), width fill
                , Font.color primaryColor, Font.size 20, Font.center
                , spacing 20
                ]
                [ link [ centerY, centerX, Font.underline ]
                    { url = "https://blog.patchgirl.io/"
                    , label = text "Blog"
                    }
                , link [ centerY, centerX, Font.underline ]
                    { url = "https://github.com/patchgirl/patchgirl"
                    , label = image [ height (px 40), paddingXY 10 0, alignRight ]
                        { src = "public/images/github_prim.svg"
                        , description = "github logo"
                        }
                    }
                ]

    in
    column [ width fill, height fill ]
        [ column [ width fill, height fill, spacing 70 ]
              [ banner
              , features
              , tryIt
              ]
        , footer
        ]



-- ** modal view


modalView : Model -> Element Msg
modalView model =
    let
        scenarioBuilderMsg msg =
            ScenarioMsg (ScenarioBuilderApp.ScenarioBuilderMsg msg)

        modalConfig =
            let
                scenarioModal =
                    \(SelectHttpRequestModal withSceneParent) ->
                        Just (Modal.map scenarioBuilderMsg (ScenarioBuilder.selectHttpRequestModal withSceneParent model.requestCollection))
            in
            Maybe.andThen scenarioModal model.whichModal

    in
    Modal.view modalConfig



-- ** notification view


notificationView : Model -> Element Msg
notificationView model =
    case model.notification of
        Just message ->
            let
                animationStyle =
                    List.map htmlAttribute (Animation.render model.notificationAnimation)
            in
            el
                ([ alignRight
                 , height (px 10)
                 ]
                    ++ animationStyle
                )
                (text message)

        Nothing ->
            none



-- * subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        (RequestCollection _ requestNodes) =
            model.requestCollection

        getRequestFiles : List RequestNode -> List RequestFileRecord
        getRequestFiles nodes =
            case nodes of
                [] ->
                    []

                requestNode :: rest ->
                    case requestNode of
                        RequestFile file ->
                            file :: getRequestFiles rest

                        RequestFolder { children } ->
                            getRequestFiles children ++ getRequestFiles rest

        requestFiles =
            getRequestFiles requestNodes

        builderMsg msg =
            BuilderAppMsg (RequestBuilderApp.BuilderMsg msg)

        buildersSubs =
            List.map (Sub.map builderMsg) (List.map RequestBuilder.subscriptions requestFiles)
    in
    Sub.batch
        ([ Animation.subscription Animate [ model.loadingAnimation ]
         , Animation.subscription Animate [ model.notificationAnimation ]
         ]
             ++ buildersSubs
             ++ [ Time.every 5000 (always CheckRunnerStatus) ]
        )
