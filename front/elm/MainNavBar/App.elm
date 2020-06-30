module MainNavBar.App exposing (..)

import Api.Converter as Client
import Api.WebGeneratedClient as Client
import Application.Type exposing (..)
import Browser.Navigation as Navigation
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html as Html
import Html.Attributes as Html
import Http as Http
import Page exposing (..)
import Util exposing (..)
import Uuid exposing (Uuid)



-- * model


type alias Model a =
    { a
        | session : Session
        , page : Page
        , showMainMenuName : Maybe MainMenuName
        , runnerRunning : Bool
    }


-- * message


type Msg
    = OpenReqPage
    | OpenEnvPage
    | OpenScenarioPage
    | OpenDocumentationPage
    | AskSignOut
    | SignOutSucceed Session
    | SignOutFailed
    | ShowMainMenuName MainMenuName
    | HideMainMenuName



-- * update


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    case msg of
        OpenReqPage ->
            let
                newModel =
                    { model | page = ReqPage Nothing Nothing }
            in
            ( newModel, Cmd.none )

        OpenEnvPage ->
            let
                newModel =
                    { model | page = EnvPage }
            in
            ( newModel, Cmd.none )

        OpenScenarioPage ->
            let
                newModel =
                    { model | page = ScenarioPage Nothing }
            in
            ( newModel, Cmd.none )

        OpenDocumentationPage ->
            let
                newModel =
                    { model | page = DocumentationPage RequestDoc }
            in
            ( newModel, Cmd.none )

        AskSignOut ->
            let
                newCmd =
                    Client.deleteApiSessionSignout "" deleteSessionSignOutResultToMsg
            in
            ( model, newCmd )

        SignOutFailed ->
            ( model, Cmd.none )

        SignOutSucceed _ ->
            let
                newMsg =
                    Navigation.load "https://patchgirl.io/"
            in
            ( model, newMsg )

        HideMainMenuName ->
            let
                newModel =
                    { model | showMainMenuName = Nothing }
            in
            ( newModel, Cmd.none )

        ShowMainMenuName mainMenuName ->
            let
                newModel =
                    { model | showMainMenuName = Just mainMenuName }
            in
            ( newModel, Cmd.none )



-- * util


deleteSessionSignOutResultToMsg : Result Http.Error Client.Session -> Msg
deleteSessionSignOutResultToMsg result =
    case result of
        Ok session ->
            let
                newSession =
                    Client.convertSessionFromBackToFront session
            in
            SignOutSucceed newSession

        Err error ->
            SignOutFailed



-- * view


view : Model a -> Element Msg
view model =
    column [ width fill, spacing 20 ]
        [ row [ width fill, Background.color primaryColor, height (px 50) ]
              [ el [ alignLeft, paddingXY 20 0 ] leftView
              , el [ centerX, centerY, height fill ] (centerView model)
              , el [ centerY, height fill, alignRight ] (rightView model)
              ]
        , case model.session of
              Visitor _ ->
                  el [ width fill
                     , paddingXY 0 10
                     , centerY
                     , Background.color secondaryColor
                     , Font.color primaryColor
                     , Font.center
                     ] (text "Sandbox mode - data will be reset every 5min, sign in to keep your data!")

              _ ->
                  none
        ]



-- ** left view


leftView : Element Msg
leftView =
    link []
        { url = href HomePage
        , label =
            image [ height (px 50) ]
                { src = "public/images/logo.png"
                , description = "logo"
                }
        }



-- ** right view


rightView : Model a -> Element Msg
rightView model =
    case model.session of
        Visitor _ ->
            visitorRightView model

        SignedUser signedUserSession ->
            signedUserRightView model signedUserSession


signedUserRightView : Model a -> SignedUserSession -> Element Msg
signedUserRightView model signedUserSession =
    row [ centerX, centerY, paddingXY 10 0, height fill ]
        [ runnerStatusView model
        , signOutView model signedUserSession
        , blogView model
        , githubLinkView model
        ]

signOutView : Model a -> SignedUserSession -> Element Msg
signOutView model { avatarUrl } =
    let
        avatar =
            image [ height (px 35), clip, Border.rounded 100 ]
                { src = avatarUrl
                , description = "avatar url"
                }
    in
    Input.button
        ([ Events.onMouseEnter (ShowMainMenuName SignOutMenu)
         , Events.onMouseLeave HideMainMenuName
         ]
            ++ mainLinkAttribute
        )
        { onPress = Just AskSignOut
        , label =
            case model.showMainMenuName of
                Just SignOutMenu ->
                    el [ below (el [ centerX, moveDown 20, Font.size 18 ] (text "Sign out")) ]
                        avatar

                _ ->
                    avatar
        }

runnerStatusView : Model a -> Element Msg
runnerStatusView model =
    let
        runnerLabel =
            case model.runnerRunning of
                True -> "PatchGirl runner app is up!"
                False -> "PatchGirl runner app is not running"

        runnerIcon =
            case model.runnerRunning of
                True -> "compare_arrows"
                False -> "compare_arrows"

        iconColor =
            case model.runnerRunning of
                True -> onColor
                False -> offColor

        runnerStatus =
            iconWithAttr { defaultIconAttribute
                             | title = ""
                             , icon =
                               case model.runnerRunning of
                                   True -> runnerIcon
                                   False -> runnerIcon
                             , iconSize = "33px"
                             , primIconColor = iconColor
                         }
    in
    el [ Events.onMouseEnter (ShowMainMenuName RunnerStatusMenu)
       , Events.onMouseLeave HideMainMenuName
       ] <|
        el [ height fill, centerY, paddingXY 15 0 ] <|
            case model.showMainMenuName of
                Just RunnerStatusMenu ->
                    el [ below (el [ centerX, moveDown 20, Font.size 18, Font.color primaryColor ] (text runnerLabel)) ]
                        runnerStatus

                _ ->
                    runnerStatus


blogView : Model a -> Element Msg
blogView model =
    link
        ([ paddingXY 20 0 ]
            ++ mainLinkAttribute
            ++ [ Events.onMouseEnter (ShowMainMenuName BlogMenu), Events.onMouseLeave HideMainMenuName ]
        )
        { url = "https://blog.patchgirl.io"
        , label =
            el [] <|
                case model.showMainMenuName of
                    Just BlogMenu ->
                        el [ below (el [ centerX, moveDown 20, Font.size 18 ] (text "Blog")) ] <|
                            iconWithTextAndColor "menu_book" "" primaryColor

                    _ ->
                        iconWithTextAndColor "menu_book" "" secondaryColor
        }


visitorRightView : Model a -> Element Msg
visitorRightView model =
    row [ centerX, centerY, paddingXY 10 0, height fill ]
        [ runnerStatusView model
        , blogView model
        , signInView model
        , githubLinkView model
        ]


signInView : Model a -> Element Msg
signInView model =
    let
        {- todo this url should be dynamic depending on the env -}
        githubOauthLink =
            "https://github.com/login/oauth/authorize?client_id=be31b06e738f5956573c&scope=user:email&redirect_uri=https://patchgirl.io"
    in
    link
        ([ Events.onMouseEnter (ShowMainMenuName SignInMenu)
         , Events.onMouseLeave HideMainMenuName
         ]
            ++ mainLinkAttribute
        )
        { url = githubOauthLink
        , label =
            case model.showMainMenuName of
                Just SignInMenu ->
                    el [ below (el [ centerX, moveDown 20, Font.size 18 ] (text "Sign in with Github")) ] <|
                        iconWithTextAndColor "vpn_key" "" primaryColor

                _ ->
                    el [ centerX ] (iconWithTextAndColor "vpn_key" "" secondaryColor)
        }


githubLinkView : Model a -> Element Msg
githubLinkView model =
    let
        githubLogoActive =
            image [ height (px 30) ]
                { src = "public/images/github.svg"
                , description = "github logo"
                }

        githubLogoInactive =
            image [ height (px 30) ]
                { src = "public/images/github_prim.svg"
                , description = "github logo"
                }
    in
    link
        ([ Events.onMouseEnter (ShowMainMenuName GithubMenu)
         , Events.onMouseLeave HideMainMenuName
         ]
            ++ mainLinkAttribute
        )
        { url = "https://github.com/patchgirl/patchgirl"
        , label =
            case model.showMainMenuName of
                Just GithubMenu ->
                    el [ below (el [ alignRight, moveDown 20, Font.size 18 ] (text "View on Github")) ] <|
                        githubLogoInactive

                _ ->
                    githubLogoActive
        }



-- ** center view


centerView : Model a -> Element Msg
centerView model =
    let
        currentDisplayedBuilderId : Maybe Uuid.Uuid
        currentDisplayedBuilderId =
            case model.page of
                ReqPage mId _ ->
                    mId

                _ ->
                    Nothing
    in
    row [ centerX, centerY, paddingXY 10 0, height fill ]
        [ link (mainLinkAttribute ++ mainLinkAttributeWhenActive OpenScenarioPage (isScenarioPage model.page))
            { url = href (ScenarioPage Nothing)
            , label = el [] (text "Scenario")
            }
        , link
            (mainLinkAttribute ++ mainLinkAttributeWhenActive OpenReqPage (isReqPage model.page))
            { url = href (ReqPage Nothing Nothing)
            , label = el [] (text "Http Request")
            }
        , link (mainLinkAttribute ++ mainLinkAttributeWhenActive OpenEnvPage (isEnvPage model.page))
            { url = href EnvPage
            , label = el [] (text "Environment")
            }
        , link (mainLinkAttribute ++ mainLinkAttributeWhenActive OpenDocumentationPage (isDocumentationPage model.page))
            { url = href (DocumentationPage RequestDoc)
            , label = el [] (text "Documentation")
            }
        ]



-- ** attribute


mainLinkAttribute : List (Attribute Msg)
mainLinkAttribute =
    [ Font.size 21
    , height fill
    , centerY
    , paddingXY 15 0
    , mouseOver
        [ Background.color secondaryColor
        , Font.color primaryColor
        ]
    , Font.color secondaryColor
    ]


mainLinkAttributeWhenActive : Msg -> Bool -> List (Attribute Msg)
mainLinkAttributeWhenActive event active =
    [ Events.onClick event ]
        ++ (case active of
                True ->
                    [ Background.color secondaryColor
                    , Font.color primaryColor
                    ]

                False ->
                    []
           )


-- ** util


isScenarioPage : Page -> Bool
isScenarioPage page =
    case page of
        ScenarioPage _ ->
            True

        _ ->
            False

isReqPage : Page -> Bool
isReqPage page =
    case page of
        ReqPage _ _ ->
            True

        _ ->
            False

isEnvPage : Page -> Bool
isEnvPage page =
    case page of
        EnvPage ->
            True

        _ ->
            False

isDocumentationPage : Page -> Bool
isDocumentationPage page =
    case page of
        DocumentationPage _ ->
            True

        _ ->
            False
