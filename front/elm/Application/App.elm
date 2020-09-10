port module Application.App exposing (..)

import Animation
import Application.Model exposing (..)
import Application.Type exposing (..)
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Navigation
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Events as Events
import Element.Border as Border
import EnvironmentEdition.App as EnvironmentEdition
import EnvironmentEdition.App2 as EnvironmentEdition2
import Element.Input as Input
import MainNavBar.App as MainNavBar
import Modal exposing (Modal(..))
import Page exposing (..)
import RequestBuilderApp.App as RequestBuilderApp
import PGBuilderApp.App as PGBuilderApp
import ScenarioBuilderApp.App as ScenarioBuilderApp
import DocumentationApp.App as DocumentationApp
import TangoScriptApp.App as TangoScriptApp
import ScenarioBuilderApp.ScenarioBuilder.App as ScenarioBuilder
import Url as Url
import Url.Parser as Url
import Util exposing (..)
import Time
import Task
import Http
import Api.RunnerGeneratedClient as Client
import Runner
import Json.Encode exposing(Value)


port sendNotification : Value -> Cmd msg


-- * model


type alias UserData =
    { session : Session
    , requestCollection : RequestCollection
    , scenarioCollection : ScenarioCollection
    , environments : List Environment
    , pgCollection : PgCollection
    }


-- * message


type Msg
    = LinkClicked UrlRequest
    | NoOp
    | ChangeDemoScene SceneToDemo
    | NextDemo
    | UrlChanged Url.Url
    | DocumentationMsg DocumentationApp.Msg
    | BuilderAppMsg RequestBuilderApp.Msg
    | PGBuilderAppMsg PGBuilderApp.Msg
    | EnvironmentEditionMsg EnvironmentEdition.Msg
    | EnvironmentEditionMsg2 EnvironmentEdition2.Msg
    | ScenarioMsg ScenarioBuilderApp.Msg
    | TangoScriptMsg TangoScriptApp.Msg
    | MainNavBarMsg MainNavBar.Msg
    | Animate Animation.Msg
    | CheckRunnerStatus
    | RunnerNotRunning
    | RunnerRunning


-- * init


init : UserData -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init { session, requestCollection, environments, scenarioCollection, pgCollection } url navigationKey =
    let
        page =
            urlToPage url

        selectedEnvironmentToEditId =
            Nothing

        selectedEnvironmentToRunIndex =
            Nothing

        initialLoadingStyle =
            Animation.style [ Animation.opacity 0 ]

        loadingAnimation =
            Animation.interrupt
                [ Animation.to
                    [ Animation.opacity 1
                    ]
                ]
                initialLoadingStyle

        sceneToDemo =
            Scene2

        initialNotificationAnimation =
            Animation.style [ Animation.opacity 0 ]

        notificationAnimation =
            Animation.interrupt
                [ Animation.to
                    [ Animation.opacity 1
                    ]
                ]
                initialNotificationAnimation

        msg =
            Task.perform (always CheckRunnerStatus) Time.now

        model =
            { session = session
            , page = page
            , url = url
            , navigationKey = navigationKey
            , loadingAnimation = loadingAnimation
            , sceneToDemo = sceneToDemo
            , notification = Nothing
            , notificationAnimation = notificationAnimation
            , whichModal = Nothing
            , showMainMenuName = Nothing
            , requestCollection = requestCollection
            , displayedPgNodeMenuId = Nothing
            , displayedPgBuilderView = LandingView DefaultView
            , displayedPgId = Nothing
            , pgCollection = pgCollection
            , pgNewNode = { name = "", parentFolderId = Nothing }
            , sqlQuery = NotEdited ""
            , pgComputation = Nothing
            , displayedRequestNodeMenuId = Nothing
            , displayedRequestBuilderView = LandingView DefaultView
            , requestNewNode = { name = "", parentFolderId = Nothing }
            , scenarioCollection = scenarioCollection
            , displayedScenarioNodeMenuId = Nothing
            , displayedScenarioId = Nothing
            , displayedSceneId = Nothing
            , script = ""
            , selectedEnvironmentToRunIndex = selectedEnvironmentToRunIndex
            , selectedEnvironmentToEditId = selectedEnvironmentToEditId
            , displayedEnvId = Nothing
            , displayedEnvironmentBuilderView = LandingView DefaultView
            , displayedEnvironmentNodeMenuId = Nothing
            , environments = environments
            , newEnvironmentName = ""
            , runnerRunning = False
            , displayedDocumentation = RequestDoc
            }
    in
    ( updateModelWithPage page model, msg )


-- * update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        handleNotification : (Model, Cmd Msg) -> (Model, Cmd Msg)
        handleNotification (newModel, cmd) =
            case newModel.notification of
                Just message ->
                    ( { newModel | notification = Nothing }
                    , Cmd.batch [ cmd, sendNotification (notificationEncoder message) ]
                    )

                Nothing ->
                    (newModel, cmd)
    in
    handleNotification <| case msg of
        NoOp ->
            (model, Cmd.none)

        ChangeDemoScene newSceneToDemo ->
            ({ model | sceneToDemo = newSceneToDemo }, Cmd.none)

        NextDemo ->
            let
                nextDemo =
                    case model.sceneToDemo of
                        Scene1 -> Scene2
                        Scene2 -> Scene3
                        Scene3 -> Scene1
            in
            ({ model | sceneToDemo = nextDemo }, Cmd.none)

        UrlChanged url ->
            let
                newModel =
                    updateModelWithPage (urlToPage url) model
            in
            (newModel, Cmd.none)

        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    let
                        newModel =
                            updateModelWithPage (urlToPage url) model

                        newMsg =
                            Navigation.pushUrl model.navigationKey <| Url.toString url
                    in
                    (newModel, newMsg)

                External url ->
                    (model, (Navigation.load url))

        BuilderAppMsg subMsg ->
            let
                ( newModel, newMsg ) =
                    RequestBuilderApp.update subMsg model
            in
            (newModel, (Cmd.map BuilderAppMsg newMsg))

        PGBuilderAppMsg subMsg ->
            let
                ( newModel, newMsg ) =
                    PGBuilderApp.update subMsg model
            in
            (newModel, (Cmd.map PGBuilderAppMsg newMsg))

        EnvironmentEditionMsg2 subMsg ->
            let
                ( newModel, newMsg ) =
                    EnvironmentEdition2.update subMsg model
            in
            (newModel, (Cmd.map EnvironmentEditionMsg2 newMsg))

        EnvironmentEditionMsg subMsg ->
            case EnvironmentEdition.update subMsg model of
                ( newModel, newSubMsg ) ->
                    (newModel, (Cmd.map EnvironmentEditionMsg newSubMsg))

        ScenarioMsg subMsg ->
            case ScenarioBuilderApp.update subMsg model of
                ( newModel, newSubMsg ) ->
                    (newModel, (Cmd.map ScenarioMsg newSubMsg))

        TangoScriptMsg subMsg ->
            case TangoScriptApp.update subMsg model of
                ( newModel, newSubMsg ) ->
                    (newModel, (Cmd.map TangoScriptMsg newSubMsg))

        MainNavBarMsg subMsg ->
            case MainNavBar.update subMsg model of
                ( newModel, newSubMsg ) ->
                    (newModel, (Cmd.map MainNavBarMsg newSubMsg))

        DocumentationMsg subMsg ->
            case DocumentationApp.update subMsg of
                newSubMsg ->
                    (model, (Cmd.map DocumentationMsg newSubMsg))

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
            (newModel, Cmd.none)

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


updateModelWithPage : Page -> Model -> Model
updateModelWithPage page model =
    let
        newModel =
            case page of
                ReqPage mId ->
                    { model | displayedRequestBuilderView = mId }

                PgPage mId ->
                    { model | displayedPgBuilderView = mId }

                EnvPage mId ->
                    { model | displayedEnvId = mId }

                EnvPage2 mId ->
                    { model | displayedEnvironmentBuilderView = mId }

                ScenarioPage mId1 mId2  ->
                    { model
                        | displayedScenarioId = mId1
                        , displayedSceneId = mId2
                    }

                DocumentationPage documentation ->
                    { model | displayedDocumentation = documentation }

                HomePage ->
                    model

                NotFoundPage ->
                    model

                TangoScriptPage ->
                    model
    in
    { newModel | page = page }


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
    Client.getApiRunnerHealth Runner.desktopRunnerUrl resultHandler


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
        appLayout : Element Msg -> Element Msg
        appLayout appView =
            column [ width fill
                   , height fill
                   , centerY
                   , spacing 10
                   ]
            [ map MainNavBarMsg (MainNavBar.view model)
            , el [ width fill, height fill ] appView
            ]

    in
        case model.page of
            HomePage ->
                homeView model

            NotFoundPage ->
                appLayout <| el [ centerY, centerX ] (text "not found")

            ReqPage _ ->
                appLayout <| map BuilderAppMsg (RequestBuilderApp.view model)

            PgPage _ ->
                appLayout <| map PGBuilderAppMsg (PGBuilderApp.view model)

            EnvPage _ ->
                appLayout <| map EnvironmentEditionMsg (EnvironmentEdition.view model)

            EnvPage2 _ ->
                appLayout <| map EnvironmentEditionMsg2 (EnvironmentEdition2.view model)

            ScenarioPage _ _ ->
                appLayout <| map ScenarioMsg (ScenarioBuilderApp.view model)

            DocumentationPage _ ->
                appLayout <| map DocumentationMsg (DocumentationApp.view model)

            TangoScriptPage ->
                appLayout <| map TangoScriptMsg (TangoScriptApp.view model)


-- ** home view


homeView : Model -> Element Msg
homeView model =
    let
        banner : Element Msg
        banner =
            row [ height (px 150), width fill
                , Background.color primaryColor
                , paddingXY 0 50
                ]
            [ column [ centerX
                     , spacing 10
                     , Font.color secondaryColor
                     , width fill
                     ]
                  [ image [ centerX, height (px 70) ]
                        { src = "public/images/logo.png"
                        , description = "logo"
                        }
                  , paragraph [ centerX, centerY, Font.size 25, Font.center ] [ text "HTTP and Postgres client to ease DB seeding" ]
                  ]
            ]

        mkScene : String -> String -> SceneToDemo -> Element Msg -> Bool -> Element Msg
        mkScene title icon sceneToDemo arrow selected =
            column [ centerX, spacing 10, Events.onMouseEnter (ChangeDemoScene sceneToDemo) ]
                [ el (box [ case selected of
                                True -> Border.color black
                                False -> Border.color white
                          , padding 20
                          , centerX
                          ]
                     ) <|
                      iconWithAttr { defaultIconAttribute
                                       | icon = icon
                                       , title = title
                                       , primIconColor = Just primaryColor
                                       , iconVerticalAlign = Just "sub"
                                   }
                , el [ centerX ] arrow
                ]

        mkMacOSWinIcon : Color -> Element Msg
        mkMacOSWinIcon color =
            el [ Background.color color
               , width (px 15)
               , height (px 15)
               , clip
               , Border.rounded 10
               ] (text " ")

        mkFeatureDescription : String -> Element Msg
        mkFeatureDescription description =
            el [ Font.size 18, width fill ] <|
                iconWithAttr { defaultIconAttribute
                                 | icon = "done_outline"
                                 , title = description
                                 , primIconColor = Just primaryColor
                             }

        leftDescriptionView : Element Msg
        leftDescriptionView =
            column [ Font.center, width fill, centerX, spacing 40 ]
                [ column [ width fill, centerX, spacing 10, Font.size 24 ]
                      [ el [ width fill ] (text "Play HTTP and SQL queries to seed your Database")
                      , el [ width fill ] (text "& Make testing fun again!")
                      ]
                , column [ width shrink, centerX, spacing 30, Font.size 20 ]
                    [ column [ width fill, centerX, spacing 20 ]
                          [ el [ centerX ] <|
                                iconWithAttr { defaultIconAttribute
                                                 | icon = "public"
                                                 , title = " Test your API"
                                                 , primIconColor = Just primaryColor
                                             }
                          , column [ spacing 10, paddingXY 30 0 ]
                              [ mkFeatureDescription " Make sure your API responds correctly"
                              , mkFeatureDescription " Check that your DB is in the correct state"
                              ]
                          ]
                    , column [ width fill, centerX, spacing 20 ]
                        [ el [ centerX, Font.size 20 ] <|
                              iconWithAttr { defaultIconAttribute
                                               | icon = "storage"
                                               , title = " Seed your DB"
                                               , primIconColor = Just primaryColor
                                           }
                        , column [ spacing 10, paddingXY 30 0 ]
                            [ mkFeatureDescription " Prepare your DB to a given state"
                            , mkFeatureDescription " Make your application testable more easily"
                            ]
                        ]
                    ]
                , link [ centerY, centerX
                     , height (px 100)
                     , width (fill |> minimum 300 |> maximum 500)
                     , Background.color primaryColor
                     , Font.color secondaryColor
                     , Font.size 25
                     , mouseOver
                           [ Background.color secondaryColor
                           , Font.color primaryColor
                           ]
                     ] { url = href (ScenarioPage model.displayedScenarioId model.displayedSceneId)
                       , label = el [ centerY, centerX ] <| text "Try it!"
                       }
                ]

        macOsView : Element Msg
        macOsView =
            column [ alignRight, centerX, width (px 800), height (px 440), spacing 30, Border.color (rgb255 172 172 172), Border.solid, Border.width 1, Border.rounded 10
                   , Border.shadow
                       { offset = (0, 0)
                       , size = 0
                       , blur = 20
                       , color = rgb255 172 172 172
                       }
                   ]
                    [ row [ Border.roundEach { topLeft = 10, topRight = 10, bottomLeft = 0, bottomRight = 0 }
                          , width fill
                          , Background.color (rgb255 200 198 201)
                          , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
                          , Border.color (rgb255 177 174 177)
                          , paddingXY 10 5
                          ]
                          [ row [ width fill ]
                                [ row [ alignLeft, spacing 10 ]
                                      [ mkMacOSWinIcon (rgb255 254 97 88)
                                      , mkMacOSWinIcon (rgb255 255 190 48)
                                      , mkMacOSWinIcon (rgb255 41 206 65)
                                      ]
                                , el [ centerX, Font.color (rgb255 77 73 77) ] (text "PatchGirl")
                                ]
                          ]
                    , wrappedRow [ width fill, paddingXY 10 10 ]
                        [ column [ alignTop, width shrink, centerX, spacing 15, paddingXY 10 0 ]
                              [ el [ width fill, Font.center, Font.size 22, Font.underline ] (text "Create a new invoice")
                              , column [ width fill, spacing 5, Font.center ]
                                  [ mkScene "Clean Database" "storage" Scene1 arrowDownwardIcon (model.sceneToDemo == Scene1)
                                  , mkScene "POST new user" "public" Scene2 arrowDownwardIcon (model.sceneToDemo == Scene2)
                                  , mkScene "POST new invoice" "public" Scene3 none (model.sceneToDemo == Scene3)
                                  ]
                              ]
                        , column ( box [ width fill, spacing 15, alignTop, padding 20, spacing 20 ] ) <|
                            case model.sceneToDemo of
                                Scene1 -> scene1View
                                Scene2 -> scene2View
                                Scene3 -> scene3View
                        ]
                    ]

        scene1View =
            [ column [ width fill, spacing 10 ]
                  [ row [ spacing 10, width fill ]
                        [ iconWithAttr { defaultIconAttribute
                                           | icon = "storage"
                                           , title = " Clean DB"
                                           , primIconColor = Just primaryColor
                                       }
                        , el [ alignRight ] clearIcon
                        ]
                  , Input.multiline [ Background.color lightGrey ]
                      { onChange = always NoOp
                      , text = "TRUNCATE user, invoice;"
                      , placeholder = Nothing
                      , label = Input.labelHidden "http gist"
                      , spellcheck = False
                      }
                ]
            ,  column [ width fill, spacing 20 ]
                [ Input.multiline []
                      { onChange = always NoOp
                      , text = ""
                      , placeholder = Nothing
                      , label = Input.labelAbove [ centerY, width fill ]
                                <| row [ width fill ]
                            [ el [ alignLeft ] (text "Prescript:")
                            ]
                      , spellcheck = False
                      }
                , Input.multiline []
                    { onChange = always NoOp
                    , text = ""
                    , placeholder = Nothing
                    , label = Input.labelAbove [ centerY, width fill ]
                              <| row [ width fill ]
                          [ el [ alignLeft ] (text "Postscript:")
                          ]
                    , spellcheck = False
                    }
                ]
            ]

        scene2View =
            [ column [ width fill, spacing 10 ]
                  [ row [ spacing 10, width fill ]
                        [ iconWithAttr { defaultIconAttribute
                                           | icon = "public"
                                           , title = "POST new user"
                                           , primIconColor = Just primaryColor
                                       }
                        , el [ alignRight ] clearIcon
                        ]
                  , Input.multiline [ Background.color lightGrey ]
                      { onChange = always NoOp
                      , text = "POST https://{{host}}/users"
                      , placeholder = Nothing
                      , label = Input.labelHidden "http gist"
                      , spellcheck = False
                      }
                ]
            ,  column [ width fill, spacing 20 ]
                [ Input.multiline []
                      { onChange = always NoOp
                      , text = """set(\"host\", \"myGreatApi.com\");"""
                      , placeholder = Nothing
                      , label = Input.labelAbove [ centerY, width fill ]
                                <| row [ width fill ]
                            [ el [ alignLeft ] (text "Prescript:")
                            ]
                      , spellcheck = False
                      }
                , Input.multiline []
                    { onChange = always NoOp
                    , text = """assertEqual(httpResponseStatus, 200);
set("userId", httpResponseAsJson["userId"]);"""
                    , placeholder = Nothing
                    , label = Input.labelAbove [ centerY, width fill ]
                              <| row [ width fill ]
                          [ el [ alignLeft ] (text "Postscript:")
                          ]
                    , spellcheck = False
                    }
                ]
            ]

        scene3View =
            [ column [ width fill, spacing 10 ]
                  [ row [ spacing 10, width fill ]
                        [ iconWithAttr { defaultIconAttribute
                                           | icon = "public"
                                           , title = "POST new invoice"
                                           , primIconColor = Just primaryColor
                                       }
                        , el [ alignRight ] clearIcon
                        ]
                  , Input.multiline [ Background.color lightGrey ]
                      { onChange = always NoOp
                      , text = "POST https://{{host}}/invoice"
                      , placeholder = Nothing
                      , label = Input.labelHidden "http gist"
                      , spellcheck = False
                      }
                ]
            ,  column [ width fill, spacing 20 ]
                [ Input.multiline []
                      { onChange = always NoOp
                      , text = """set("host", "myGreatApi.com");
set("userId", get("userId"));"""
                      , placeholder = Nothing
                      , label = Input.labelAbove [ centerY, width fill ]
                                <| row [ width fill ]
                            [ el [ alignLeft ] (text "Prescript:")
                            ]
                      , spellcheck = False
                      }
                , Input.multiline []
                    { onChange = always NoOp
                    , text = """assertEqual(httpResponseStatus, 200);"""
                    , placeholder = Nothing
                    , label = Input.labelAbove [ centerY, width fill ]
                              <| row [ width fill ]
                          [ el [ alignLeft ] (text "Postscript:")
                          ]
                    , spellcheck = False
                    }
                ]
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
        [ column [ width fill, height fill, spacing 50 ]
              [ banner
              , wrappedRow [ width fill, centerX, spacing 50, paddingXY 20 0 ]
                  [ leftDescriptionView
                  , macOsView
                  ]
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
                    \(SelectNewSceneModal withSceneParent) ->
                        Just (Modal.map scenarioBuilderMsg (ScenarioBuilder.selectSceneModal withSceneParent model.requestCollection model.pgCollection))
            in
            Maybe.andThen scenarioModal model.whichModal

    in
    Modal.view modalConfig


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
                        File file ->
                            file :: getRequestFiles rest

                        Folder { children } ->
                            let
                                (RequestChildren c) =
                                    children
                            in
                            getRequestFiles c ++ getRequestFiles rest

        requestFiles =
            getRequestFiles requestNodes

    in
    Sub.batch
        ([ Animation.subscription Animate [ model.loadingAnimation ]
         , Animation.subscription Animate [ model.notificationAnimation ]
         ]
             ++ [ Time.every 5000 (always CheckRunnerStatus) ]
             ++ [ Time.every 5000 (always NextDemo) ]
        )
