module MainNavBar.App exposing (..)

import Element exposing (..)
import Element.Background as Background
import Browser.Navigation as Navigation
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Element.Font as Font
import Uuid
import ViewUtil exposing (..)

import Html as Html
import Html.Attributes as Html
import Application.Type exposing (..)
import Api.Generated as Client
import Api.Converter as Client
import Http as Http
import Page exposing(..)


-- * model


type alias Model a =
    { a
      | session : Session
      , page : Page
      , showMainMenuName : Maybe MainMenuName
    }


-- * message


type Msg
    = OpenReqPage
    | OpenEnvPage
    | AskSignOut
    | SignOutSucceed Session
    | SignOutFailed
    | ShowMainMenuName MainMenuName
    | HideMainMenuName


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
        OpenReqPage ->
            let
                newModel =
                    { model | page = ReqPage Nothing }
            in
                (newModel, Cmd.none)

        OpenEnvPage ->
            let
                newModel =
                    { model | page = EnvPage }
            in
                (newModel, Cmd.none)

        AskSignOut ->
            let
                newCmd =
                    Client.deleteApiSessionSignout "" deleteSessionSignOutResultToMsg
            in
                (model, newCmd)

        SignOutFailed ->
            (model, Cmd.none)

        SignOutSucceed _ ->
            let
                newMsg =
                    Navigation.load "https://dev.patchgirl.io/"
            in
                (model, newMsg)

        HideMainMenuName ->
            let
                newModel =
                    { model | showMainMenuName = Nothing }
            in
                (newModel, Cmd.none)

        ShowMainMenuName mainMenuName ->
            let
                newModel =
                    { model | showMainMenuName = Just mainMenuName }
            in
                (newModel, Cmd.none)


-- * util


deleteSessionSignOutResultToMsg : Result Http.Error Client.Session -> Msg
deleteSessionSignOutResultToMsg result =
    case result of
        Ok session ->
            let
                newSession = Client.convertSessionFromBackToFront session
            in
                SignOutSucceed newSession

        Err error ->
            SignOutFailed



-- * view


view : Model a -> Element Msg
view model =
    row [ width fill, Background.color primaryColor, height (px 50) ]
        [ el [ alignLeft, paddingXY 20 0 ] leftView
        , el [ centerX, centerY, height fill ] (centerView model)
        , el [ centerY, height fill, alignRight ] (rightView model)
        ]

-- ** left view


leftView : Element Msg
leftView =
    let
        linkContent =
            html <|
                Html.span [ Html.style "color" (colorToString secondaryColor)
                          , Html.style "font-size" "30px"
                          ]
                    [ Html.i
                          [ Html.class "material-icons"
                          , Html.style "font-size" "30px"
                          , Html.style "vertical-align" "bottom"
                          ]
                          [ Html.text "call_split" ]
                    , Html.text "PatchGirl"
                    ]
    in
        link [] { url = href HomePage
                , label = linkContent
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
        [ signOutView model signedUserSession
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
        Input.button ( [ Events.onMouseEnter (ShowMainMenuName SignOutMenu)
                       , Events.onMouseLeave HideMainMenuName
                       ] ++ mainLinkAttribute
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

blogView : Model a -> Element Msg
blogView model =
    link ( [ paddingXY 20 0 ]
           ++ mainLinkAttribute
           ++ [ Events.onMouseEnter (ShowMainMenuName BlogMenu), Events.onMouseLeave HideMainMenuName ]
         )
    { url = "https://blog.patchgirl.io"
    , label =
        el [] <|
            case model.showMainMenuName of
                Just BlogMenu ->
                    el [ below (el [ centerX, moveDown 20, Font.size 18 ] (text "Blog")) ]
                        <| iconWithTextAndColor "menu_book" "" primaryColor

                _ ->
                    iconWithTextAndColor "menu_book" "" secondaryColor
    }

visitorRightView : Model a -> Element Msg
visitorRightView model =
    row [ centerX, centerY, paddingXY 10 0, height fill ]
        [ blogView model
        , signInView model
        , githubLinkView model
        ]

signInView : Model a -> Element Msg
signInView model =
    let
        githubOauthLink = "https://github.com/login/oauth/authorize?client_id=aca37e4fb27953755695&scope=user:email&redirect_uri=https://dev.patchgirl.io"
    in
        link ( [ Events.onMouseEnter (ShowMainMenuName SignInMenu)
               , Events.onMouseLeave HideMainMenuName
               ] ++ mainLinkAttribute
             )
        { url = githubOauthLink
        , label =
            case model.showMainMenuName of
                Just SignInMenu ->
                    el [ below (el [ centerX, moveDown 20, Font.size 18 ] (text "Sign in with Github")) ]
                        <| iconWithTextAndColor "vpn_key" "" primaryColor

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
        link ( [ Events.onMouseEnter (ShowMainMenuName GithubMenu)
               , Events.onMouseLeave HideMainMenuName
               ] ++ mainLinkAttribute
             )
        { url = "https://github.com/patchgirl/patchgirl"
        , label =
            case model.showMainMenuName of
                Just GithubMenu ->
                    el [ below (el [ alignRight, moveDown 20, Font.size 18 ] (text "View on Github")) ]
                        <| githubLogoInactive

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
                ReqPage mId ->
                    mId

                _ ->
                    Nothing
    in
        row [ centerX, centerY, paddingXY 10 0, height fill ]
            [ link (mainLinkAttribute ++ (mainLinkAttributeWhenActive model OpenReqPage (ReqPage currentDisplayedBuilderId)))
                  { url = "#req"
                  , label = el [] (text "Req")
                  }
            , link (mainLinkAttribute ++ (mainLinkAttributeWhenActive model OpenEnvPage EnvPage))
                { url = "#env"
                , label = el [] (text "Env")
                }
            ]


-- ** attribute


mainLinkAttribute : List (Attribute Msg)
mainLinkAttribute =
    [ Font.size 21
    , height fill
    , centerY
    , paddingXY 15 0
    , mouseOver [ Background.color secondaryColor
                , Font.color primaryColor
                ]
    , Font.color secondaryColor
    ]

mainLinkAttributeWhenActive : Model a -> Msg -> Page -> List (Attribute Msg)
mainLinkAttributeWhenActive model event page =
    [ Events.onClick event ] ++
        case model.page == page of
            True -> [ Background.color secondaryColor
                    , Font.color primaryColor
                    ]
            False ->
                []
