module SignIn.App exposing (..)

import Application.Type exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Html exposing (Html)
import ViewUtil exposing (..)
import Api.Generated as Client
import Api.Converter as Client
import Http as Http


-- * model


type alias Model a =
    { a | id: Int
        , csrfToken: String
        , signInEmail: String
        , signInPassword: String
    }


-- * message


type Msg
    = ChangeEmailSignIn String
    | ChangePasswordSignIn String
    | AskSignIn
    | SignInSucceed Session
    | SignInFailed


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of

        ChangeEmailSignIn newSignInEmail ->
            let
                newModel =
                    { model | signInEmail = newSignInEmail }

            in
                (newModel, Cmd.none)

        ChangePasswordSignIn newSignInPassword ->
            let
                newModel =
                    { model | signInPassword = newSignInPassword }
            in
                (newModel, Cmd.none)

        AskSignIn ->
            let
                login =
                    { email = Client.CaseInsensitive model.signInEmail
                    , password = model.signInPassword
                    }

                newCmd =
                    Client.postSessionSignin "" login postSessionSignInResultToMsg
            in
                (model, newCmd)

        SignInFailed ->
            (model, Cmd.none)

        SignInSucceed _ ->
            Debug.todo "unreachable state: sign in"


-- * util


postSessionSignInResultToMsg : Result Http.Error Client.Session -> Msg
postSessionSignInResultToMsg result =
    case result of
        Ok session ->
            let
                newSession = Client.convertSessionFromBackToFront session
            in
                SignInSucceed newSession

        Err _ ->
            SignInFailed


-- * view


view : Model a -> Element Msg
view model =
    let
        labelInputAttributes =
            [ centerY
            , width (fill |> minimum 100)
            , alignRight
            ]

        loginInput =
            Input.username []
                { onChange = ChangeEmailSignIn
                , text = model.signInEmail
                , placeholder = Just <| Input.placeholder [] (text "email")
                , label = Input.labelLeft labelInputAttributes (text "Email:")
                }

        passwordInput =
            Input.currentPassword []
                { onChange = ChangePasswordSignIn
                , text = model.signInPassword
                , placeholder = Just <| Input.placeholder [] (text "password")
                , label = Input.labelLeft labelInputAttributes (text "Password:")
                , show = False
                }

        submitButton =
            Input.button [ Border.solid
                         , Border.color secondaryColor
                         , Border.width 1
                         , Border.rounded 5
                         , alignBottom
                         , Background.color secondaryColor
                         , paddingXY 10 10
                         , centerX
                         ]
                { onPress = Just AskSignIn
                , label =
                    row [ centerX ]
                        [ el [] (text "Sign in")
                        ]
                }
    in
        column [ centerX, spacing 20 ]
            [ loginInput
            , passwordInput
            , submitButton
            ]
