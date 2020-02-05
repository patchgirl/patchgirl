module MainNavBar.App exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font

import ViewUtil exposing (..)

import Html as Html
import Html.Attributes as Html
import InitializedApplication.Model exposing (..)
import Application.Type exposing (..)
import Api.Generated as Client
import Api.Converter as Client
import Http as Http
import Page exposing(..)
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
                    { model | page = ReqPage }
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
                    , Html.text "ApiTester"
                    ]
    in
        link [] { url = href HomePage
                , label = linkContent
                }

rightView : Model a -> Element Msg
rightView model =
    case model.session of
        Visitor _ ->
            visitorRightView model

        SignedUser _ ->
            signedUserRightView model

visitorRightView : Model a -> Element Msg
visitorRightView model =
    row []
        [ link ([ paddingXY 20 0 ] ++ (mainLinkAttribute model OpenSignInPage SignInPage))
              { url = "#signIn"
              , label = text "Sign in"
              }
        , link ([ paddingXY 20 0 ] ++ (mainLinkAttribute model OpenSignUpPage SignUpPage))
            { url = "#signUp"
            , label = text "Sign up"
            }
        ]

signedUserRightView : Model a -> Element Msg
signedUserRightView model =
    row [ spacing 10 ]
        [ link (linkAttribute model AskSignOut)
            { url = href SignOutPage
            , label = text "Sign out"
            }
        ]

centerView : Model a -> Element Msg
centerView model =
    row [ centerX, paddingXY 10 0, centerY ]
        [ link (mainLinkAttribute model OpenReqPage ReqPage) { url = "#req", label = text "Req" }
        , link (mainLinkAttribute model OpenEnvPage EnvPage) { url = "#env", label = text "Env" }
        ]


linkAttribute : Model a -> Msg -> List (Attribute Msg)
linkAttribute model msg =
    let
        activeAttribute =
            [ Background.color <| secondaryColor
            , Font.color <| primaryColor
            ]


    in
        [ Events.onClick msg
        , Font.size 21
        , Font.color <| secondaryColor
        , paddingXY 15 19
        , mouseOver activeAttribute
        ]



mainLinkAttribute : Model a -> Msg -> Page -> List (Attribute Msg)
mainLinkAttribute model event page =
            let
                activeAttribute =
                    [ Background.color <| secondaryColor
                    , Font.color <| primaryColor
                    ]

                passiveAttribute =
                    [ Font.color <| secondaryColor
                    ]

                activeOrPassiveAttribute =
                    case model.page == page of
                        True -> activeAttribute
                        False -> passiveAttribute

            in
                [ Events.onClick event
                , Font.size 21
                , paddingXY 15 19
                , mouseOver activeAttribute
                ] ++ activeOrPassiveAttribute


view : Model a -> Element Msg
view model =
    el [ width fill, Background.color primaryColor ] <|
        row [ width fill]--, explain Debug.todo]
            [ el [ alignLeft, paddingXY 20 0 ] leftView
            , el [ centerX ] <| centerView model
            , el [ alignRight ] (rightView model)
            ]
