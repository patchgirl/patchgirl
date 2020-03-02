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
import InitializedApplication.Model exposing (..)
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

blogView : Element Msg
blogView =
    link ([ paddingXY 20 0 ] ++ mainLinkAttribute)
              { url = "https://blog.patchgirl.io"
              , label = el [] (text "Blog")
              }

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

visitorRightView : Model a -> Element Msg
visitorRightView model =
    row [ centerX, centerY, paddingXY 10 0, height fill ]
        [ link (mainLinkAttribute ++ (mainLinkAttributeWhenActive model OpenSignInPage SignInPage))
              { url = "#signIn"
              , label = el [] (text "Sign in")
              }
        , link (mainLinkAttribute ++ (mainLinkAttributeWhenActive model OpenSignUpPage SignUpPage))
            { url = "#signUp"
            , label = el [] (text "Sign up")
            }
        , blogView
        , githubLinkView
        ]

signedUserRightView : Model a -> Element Msg
signedUserRightView model =
    row [ centerX, centerY, paddingXY 10 0, height fill ]
        [ link (linkAttribute model AskSignOut)
            { url = href SignOutPage
            , label = el [] (text "Sign out")
            }
        , blogView
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
