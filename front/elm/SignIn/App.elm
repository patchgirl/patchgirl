module SignIn.App exposing (..)

import Application.Type exposing (..)

import Element exposing (..)
import SignIn.Model exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Html exposing (Html)
import ViewUtil exposing (..)
import Api.Generated as Client
import Api.Converter as Client
import Http as Http
import Maybe.Extra as Maybe

-- * model


type alias Model a =
    { a | id: Int
        , csrfToken: String
        , signInEmail: String
        , signInPassword: String
        , signInErrors: List String
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
                    { model
                        | signInEmail = newSignInEmail
                        , signInErrors = []
                    }

            in
                (newModel, Cmd.none)

        ChangePasswordSignIn newSignInPassword ->
            let
                newModel =
                    { model
                        | signInPassword = newSignInPassword
                        , signInErrors = []
                    }
            in
                (newModel, Cmd.none)

        AskSignIn ->
            let
                signIn =
                    { email = Client.CaseInsensitive model.signInEmail
                    , password = model.signInPassword
                    }

                newCmd =
                    Client.postApiSessionSignin "" (Client.convertSignInFromFrontToBack signIn) postSessionSignInResultToMsg

                newModel =
                    { model | signInErrors = signInErrors signIn }
            in
                case List.isEmpty newModel.signInErrors of
                    True ->
                        (newModel, newCmd)

                    False ->
                        (newModel, Cmd.none)

        SignInFailed ->
            let
                newModel =
                    { model | signInErrors = [] }
            in
                (newModel, Cmd.none)

        SignInSucceed _ ->
            Debug.todo "unreachable state: sign in"


-- * util


signInErrors : SignIn -> List String
signInErrors { email, password } =
    let
        (Client.CaseInsensitive ciEmail) = email

        passwordError : Maybe String
        passwordError =
            if String.isEmpty password then
                Just "Password can not be empty"
            else
                Nothing

        emailError : Maybe String
        emailError =
            if String.isEmpty ciEmail then
                Just "Email can not be empty"
            else
                Nothing
    in
        Maybe.values [ passwordError
                     , emailError
                     ]

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
            Input.username [ onEnter AskSignIn ]
                { onChange = ChangeEmailSignIn
                , text = model.signInEmail
                , placeholder = Just <| Input.placeholder [] (text "email")
                , label = Input.labelLeft labelInputAttributes (text "Email:")
                }

        passwordInput =
            Input.currentPassword [ onEnter AskSignIn ]
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

        showErrors =
            case model.signInErrors of
                [] ->
                    none

                errors ->
                    column [] <| List.map (\error -> text error) errors
    in
        column [ centerX, spacing 20 ]
            [ loginInput
            , passwordInput
            , showErrors
            , submitButton
            ]
