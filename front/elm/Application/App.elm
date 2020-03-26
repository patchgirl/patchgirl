module Application.App exposing (..)

import Http as Http
import Api.Generated as Client
import Api.Converter as Client
import Element exposing (..)
import Element.Font as Font
import Element.Background as Background
import ViewUtil exposing (..)
import Html as Html
import Uuid

import SignIn.App as SignIn
import MainNavBar.App as MainNavBar
import Application.Type exposing (..)
import Url as Url
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Navigation
import Tuple as Tuple
import Page exposing (..)
import Url.Parser as Url exposing ((</>))
import Animation
import BuilderApp.App as BuilderApp
import BuilderApp.Builder.App as Builder
import SignIn.App as SignIn
import SignUp.App as SignUp
import InitializePassword.App as InitializePassword
import EnvironmentEdition.App as EnvironmentEdition
import EnvironmentToRunSelection.App as EnvSelection
import BuilderApp.BuilderTree.App as BuilderTree
import Application.Model exposing (..)


-- ** model


type alias UserData =
    { session : Session
    , requestCollection : RequestCollection
    , environments : List Environment
    }


-- ** message


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url.Url
    | ServerError Http.Error
    | BuilderTreeMsg BuilderTree.Msg
    | BuilderAppMsg BuilderApp.Msg
    | EnvironmentEditionMsg EnvironmentEdition.Msg
    | MainNavBarMsg MainNavBar.Msg
    | SignInMsg SignIn.Msg
    | SignUpMsg SignUp.Msg
    | InitializePasswordMsg InitializePassword.Msg
    | Animate Animation.Msg


-- ** init


init : UserData -> Url.Url -> Navigation.Key -> (Model, Cmd Msg)
init { session, requestCollection, environments } url navigationKey =
    let
        page =
            urlToPage url

        displayedRequestNodeMenuId =
            Nothing

        selectedEnvironmentToEditId =
            Just 0

        selectedEnvironmentToRunIndex =
            Just 0

        initialLoadingStyle =
            Animation.style [ Animation.opacity 0 ]

        loadingStyle =
            Animation.interrupt
                [ Animation.to
                      [ Animation.opacity 1
                      ]
                ] initialLoadingStyle

        model =
            { session = session
            , page = page
            , url = url
            , navigationKey = navigationKey
            , loadingStyle = loadingStyle
            , showMainMenuName = Nothing
            , initializePassword1 = ""
            , initializePassword2 = ""
            , initializePasswordState = InitialPasswordState
            , displayedRequestNodeMenuId = displayedRequestNodeMenuId
            , requestCollection = requestCollection
            , selectedEnvironmentToRunIndex = selectedEnvironmentToRunIndex
            , selectedEnvironmentToEditId = selectedEnvironmentToEditId
            , environments = environments
            }
    in
        (model, Cmd.none)


-- ** update


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
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

        BuilderTreeMsg subMsg ->
            let
                (newModel, newSubMsg) = BuilderTree.update subMsg model
            in
                (newModel, Cmd.map BuilderTreeMsg newSubMsg)

        BuilderAppMsg subMsg ->
            let
                (newModel, newMsg) = BuilderApp.update subMsg model
            in
                (newModel, Cmd.map BuilderAppMsg newMsg)

        EnvironmentEditionMsg subMsg ->
            case EnvironmentEdition.update subMsg model of
                (newModel, newSubMsg) ->
                    (newModel, Cmd.map EnvironmentEditionMsg newSubMsg)

        MainNavBarMsg subMsg ->
            case MainNavBar.update subMsg model of
                (newModel, newSubMsg) ->
                    (newModel, Cmd.map MainNavBarMsg newSubMsg)

        SignInMsg subMsg ->
            case model.session of
                Visitor visitorSession ->
                    case SignIn.update subMsg visitorSession of
                        (newVisitorSession, newSubMsg) ->
                            let newModel =
                                    { model | session = Visitor newVisitorSession }
                            in
                                (newModel, Cmd.map SignInMsg newSubMsg)

                _ ->
                    Debug.todo "cannot sign in if not a visitor"

        SignUpMsg subMsg ->
            case model.session of
                Visitor visitorSession ->
                    case SignUp.update subMsg visitorSession of
                        (newVisitorSession, newSubMsg) ->
                            let newModel =
                                    { model | session = Visitor newVisitorSession }
                            in
                                (newModel, Cmd.map SignUpMsg newSubMsg)

                _ ->
                    Debug.todo "cannot sign up if not a visitor"

        InitializePasswordMsg subMsg ->
            case InitializePassword.update subMsg model of
                (newModel, newSubMsg) ->
                    (newModel, Cmd.map InitializePasswordMsg newSubMsg)

        Animate subMsg ->
            let
                newLoadingStyle =
                    Animation.update subMsg model.loadingStyle

                newModel =
                    { model
                        | loadingStyle = newLoadingStyle
                    }
            in
                (newModel, Cmd.none)


-- ** view


view : Model -> Browser.Document Msg
view model =
    let
        loadingStyle =
            List.map htmlAttribute (Animation.render model.loadingStyle)

        bodyAttr =
            [ Background.color lightGrey ] ++ loadingStyle

        body =
            layout bodyAttr (mainView model)
    in
        { title = "PatchGirl"
        , body = [body]
        }


mainView : Model -> Element Msg
mainView model =
    let
        builderView =
            map BuilderAppMsg (BuilderApp.view model)
    in
        column [ width fill, height fill
               , centerY
               , spacing 30
               ]
        [ map MainNavBarMsg (MainNavBar.view model)
        , el [ width fill ] <|
            case model.page of
                HomePage -> builderView
                NotFoundPage -> el [ centerY, centerX ] (text "not found")
                ReqPage mId -> builderView
                EnvPage -> map EnvironmentEditionMsg (EnvironmentEdition.view model)
        ]


-- ** subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        (RequestCollection _ requestNodes) =
            model.requestCollection

        getRequestFiles : List RequestNode -> List File
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
            BuilderAppMsg (BuilderApp.BuilderMsg msg)

        buildersSubs =
            List.map (Sub.map builderMsg) (List.map Builder.subscriptions requestFiles)
    in
        Sub.batch
            ( [ Animation.subscription Animate [ model.loadingStyle ]
              ] ++ buildersSubs
            )
