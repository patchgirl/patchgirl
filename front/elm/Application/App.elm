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


-- * model


type alias UserData =
    { session : Session
    , requestCollection : RequestCollection
    , scenarioCollection : ScenarioCollection
    , environments : List Environment
    , config : Config
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



-- * init


init : UserData -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init { session, requestCollection, environments, scenarioCollection, config } url navigationKey =
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
            , config = config
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
        gifs : Element Msg
        gifs =
            wrappedRow [ width fill, spacing 1, centerX, Font.center ]
                [ column [ alignTop, width fill ]
                      [ el [ width fill ] (text "Create & store variables to reuse in your http request")
                      , image [ centerX ] { src = "/public/images/create_variables.gif", description = "create variables" }
                      ]
                , column [ alignTop, width fill ]
                      [ el [ width fill ] (text "Create variables to reuse anywhere")
                      , image [ centerX ] { src = "/public/images/create_request.gif", description = "create http request" }
                      ]
                , column [ alignTop, width fill ]
                      [ el [ width fill ] (text "Play scenario of http requests")
                      , image [ centerX ] { src = "/public/images/run_scenario.gif", description = "play scenario of http requests" }
                      ]
                ]
    in
    column [ centerX, spacing 50, moveDown 100 ]
        [ column [ centerX, spacing 20 ]
             [ el [ centerX, Font.size 30 ] (text "PatchGirl")
             , el [ centerX, Font.size 25 ] (text "An open source rest-client to play scenarios of http requests")
             ]
        , gifs
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
        )
