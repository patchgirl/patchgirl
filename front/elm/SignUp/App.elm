module SignUp.App exposing (..)

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
    { a | signUpEmail: String
        , signUpPassword: String
    }


-- * message


type Msg
    = ChangeEmailSignUp String
    | ChangePasswordSignUp String
    | AskSignUp
    | SignUpSucceed Session
    | SignUpFailed


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of

        ChangeEmailSignUp newSignUpEmail ->
            let
                newModel =
                    { model | signUpEmail = newSignUpEmail }

            in
                (newModel, Cmd.none)

        ChangePasswordSignUp newSignUpPassword ->
            let
                newModel =
                    { model | signUpPassword = newSignUpPassword }
            in
                (newModel, Cmd.none)

        AskSignUp ->
            let
                login =
                    { email = Client.CaseInsensitive model.signUpEmail
                    , password = model.signUpPassword
                    }

                newCmd =
                    Client.postSessionSignin "" login postSessionSignUpResultToMsg
            in
                (model, newCmd)

        SignUpFailed ->
            (model, Cmd.none)

        SignUpSucceed _ ->
            Debug.todo "unreachable state: sign in"


-- * util


postSessionSignUpResultToMsg : Result Http.Error Client.Session -> Msg
postSessionSignUpResultToMsg result =
    case result of
        Ok session ->
            let
                newSession = Client.convertSessionFromBackToFront session
            in
                SignUpSucceed newSession

        Err _ ->
            SignUpFailed


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
                { onChange = ChangeEmailSignUp
                , text = model.signUpEmail
                , placeholder = Just <| Input.placeholder [] (text "email")
                , label = Input.labelLeft labelInputAttributes (text "Email:")
                }

        passwordInput =
            Input.newPassword []
                { onChange = ChangePasswordSignUp
                , text = model.signUpPassword
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
                { onPress = Just AskSignUp
                , label =
                    row [ centerX ]
                        [ el [] (text "Sign up")
                        ]
                }
    in
        column [ centerX, spacing 20 ]
            [ loginInput
            , passwordInput
            , submitButton
            ]
