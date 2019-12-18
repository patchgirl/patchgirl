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
import Regex as Regex


-- * model


type alias Model a =
    { a
        | signUpEmail : String
        , signUpError : Maybe String
        , signUpMessage : Maybe String
    }


-- * message


type Msg
    = ChangeEmailSignUp String
    | AskSignUp
    | SignUpSucceed
    | SignUpFailed


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of

        ChangeEmailSignUp newSignUpEmail ->
            let
                newModel =
                    { model
                        | signUpEmail = newSignUpEmail
                        , signUpError = Nothing
                        , signUpMessage = Nothing
                    }

            in
                (newModel, Cmd.none)

        AskSignUp ->
            let
                signUp =
                    { email = model.signUpEmail
                    }

                newCmd =
                    Client.postSessionSignup "" (Client.convertSignUpFromFrontToBack signUp) postSessionSignUpResultToMsg

                newModel =
                    { model
                        | signUpError = Just "invalid email"
                        , signUpMessage = Nothing
                    }
            in
                case validEmail signUp.email of
                    True ->
                        (model, newCmd)

                    False ->
                        (newModel, Cmd.none)

        SignUpFailed ->
            let
                newModel =
                    { model
                        | signUpError = Just "Could not sign up maybe the email is not valid/already taken ?"
                        , signUpMessage = Nothing
                    }
            in
                (newModel, Cmd.none)

        SignUpSucceed ->
            let
               newModel =
                   { model
                       | signUpError = Nothing
                       , signUpMessage = Just "Sign up succeeded, check your email to create your account"
                   }
            in
                (newModel, Cmd.none)


-- * util


validEmail : String -> Bool
validEmail email =
    let
        emailRegex : Regex.Regex
        emailRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
    in
        Regex.contains emailRegex email


postSessionSignUpResultToMsg : Result Http.Error () -> Msg
postSessionSignUpResultToMsg result =
    case Debug.log "result" result of
        Ok () ->
            SignUpSucceed

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
            Input.username [ onEnter AskSignUp ]
                { onChange = ChangeEmailSignUp
                , text = model.signUpEmail
                , placeholder = Just <| Input.placeholder [] (text "email")
                , label = Input.labelLeft labelInputAttributes (text "Email:")
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

        showError =
            case model.signUpError of
                Nothing ->
                    none

                Just error  ->
                    el [ centerX] (text error)

        showMessage =
            case model.signUpMessage of
                Nothing ->
                    none

                Just message ->
                    el [ centerX] (text message)
    in
        column [ centerX, spacing 20 ]
            [ loginInput
            , showError
            , showMessage
            , submitButton
            ]
