module MainNavBar.App exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
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
    | OpenSignInPage
    | OpenSignUpPage
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

        OpenSignInPage ->
            let
                newModel =
                    { model | page = SignInPage }
            in
                (newModel, Cmd.none)

        OpenSignUpPage ->
            let
                newModel =
                    { model | page = SignUpPage }
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
            Debug.todo "unreachable state: sign out"

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

        SignedUser _ ->
            signedUserRightView model

blogView : Model a -> Element Msg
blogView model =
    link ( [ paddingXY 20 0 ]
           ++ mainLinkAttribute
           ++ [ Events.onMouseEnter (ShowMainMenuName BlogMenu), Events.onMouseLeave HideMainMenuName ]
         )
              { url = "https://blog.patchgirl.io"
              , label = el [] <|
                  case model.showMainMenuName of
                      Just BlogMenu ->
                          el [ below (el [ centerX, moveDown 20, Font.size 18 ] (text "Blog")) ]
                              <| iconWithTextAndColor "menu_book" "" primaryColor

                      _ ->
                          iconWithTextAndColor "menu_book" "" secondaryColor
              }

visitorRightView : Model a -> Element Msg
visitorRightView model =
    let
        githubOauthLink = "https://github.com/login/oauth/authorize?client_id=aca37e4fb27953755695&scope=user:email&redirect_uri=https://dev.patchgirl.io/#/oauth-callback"
    in
        row [ centerX, centerY, paddingXY 10 0, height fill ]
            [ blogView model
            , link ( mainLinkAttribute
                     ++ (mainLinkAttributeWhenActive model OpenSignInPage SignInPage)
                     ++ [ Events.onMouseEnter (ShowMainMenuName SignInMenu)
                        , Events.onMouseLeave HideMainMenuName
                        ]
                   )
                  { url = githubOauthLink
                  , label =
                      el [] <|
                          case model.showMainMenuName of
                              Just SignInMenu ->
                                  el [ below (el [ centerX, moveDown 20, Font.size 18 ] (text "Sign in with Github")) ]
                                      <| iconWithTextAndColor "vpn_key" "" primaryColor

                              _ ->
                                  iconWithTextAndColor "vpn_key" "" secondaryColor
                  }
            , githubLinkView
            ]

githubLinkView : Element Msg
githubLinkView =
    link [ paddingXY 10 0 ]
        { url = "https://github.com/patchgirl/patchgirl"
        , label =
            image [ height (px 30) ]
                { src = "public/images/github.svg"
                , description = "github logo"
                }
        }

signedUserRightView : Model a -> Element Msg
signedUserRightView model =
    row [ centerX, centerY, paddingXY 10 0, height fill ]
        [ link (linkAttribute model AskSignOut)
            { url = href SignOutPage
            , label = el [] (text "Sign out")
            }
        , blogView model
        , githubLinkView
        ]


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


linkAttribute : Model a -> Msg -> List (Attribute Msg)
linkAttribute model msg =
    let
        activeAttribute =
            [ Background.color <| secondaryColor
            , Font.color <| primaryColor
            ]
    in
        [ Events.onClick msg
        , Font.size font
        , Font.color <| secondaryColor
        , paddingXY 15 padding
        , mouseOver activeAttribute
        ]

font = 21
padding = 0

mainLinkAttribute : List (Attribute Msg)
mainLinkAttribute =
    let
        activeAttribute =
            [ Background.color secondaryColor
            , Font.color primaryColor
            ]
    in
        [ Font.size font
        , height fill
        , centerY
        , paddingXY 15 padding
        , mouseOver activeAttribute
        , Font.color secondaryColor
        ]

mainLinkAttributeWhenActive : Model a -> Msg -> Page -> List (Attribute Msg)
mainLinkAttributeWhenActive model event page =
            let
                activeAttribute =
                    [ Background.color secondaryColor
                    , Font.color primaryColor
                    ]

                passiveAttribute =
                    [ Font.color secondaryColor
                    ]

                activeOrPassiveAttribute =
                    case model.page == page of
                        True -> activeAttribute
                        False -> passiveAttribute

            in
                [ Events.onClick event
                ] ++ activeOrPassiveAttribute
