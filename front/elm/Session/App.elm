module Session.App exposing (..)

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
    { a
      | session : Session
      , emailSignin : Editable String
      , passwordSignin : Editable String
    }


type SigninResult
    = SigninSucceed Session
    | SigninFailed


-- * message


type Msg
    = ChangeEmailSignin String
    | ChangePasswordSignin String
    | AskSignIn
    | GotSigninResult SigninResult


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of

        ChangeEmailSignin emailSignin ->
            let
                oldEmailSignin = model.emailSignin
                newEmailSignin = changeEditedValue emailSignin oldEmailSignin
                newModel = { model | emailSignin = newEmailSignin }
            in
                (newModel, Cmd.none)

        ChangePasswordSignin passwordSignin ->
            let
                oldPasswordSignin = model.passwordSignin
                newPasswordSignin = changeEditedValue passwordSignin oldPasswordSignin
                newModel = { model | passwordSignin = newPasswordSignin }
            in
                (newModel, Cmd.none)

        AskSignIn ->
            let
                login =
                    { email = Client.CaseInsensitive <| editedOrNotEditedValue model.emailSignin
                    , password = editedOrNotEditedValue model.passwordSignin
                    }

                newCmd =
                    Client.postSessionLogin "" login postSessionLoginResultToMsg
            in
                (model, newCmd)

        GotSigninResult signinResult ->
            case signinResult of
                SigninSucceed newSession ->
                    let
                        newModel =
                            { model | session = newSession }
                    in
                        (newModel, Cmd.none)

                SigninFailed ->
                    (model, Cmd.none)


-- * util


postSessionLoginResultToMsg : Result Http.Error Client.Session -> Msg
postSessionLoginResultToMsg result =
    case result of
        Ok session ->
            let
                newSession = Client.convertSessionFromBackToFront session
            in
                GotSigninResult (SigninSucceed newSession)

        Err error ->
            GotSigninResult SigninFailed


-- * view


view : Model a -> Element Msg
view model =
    case model.session of
        Visitor {} ->
            visitorView model

        SignedUser {} ->
            signedUserView model

signedUserView : Model a -> Element Msg
signedUserView model =
    none


visitorView : Model a -> Element Msg
visitorView model =
    let
        labelInputAttributes =
            [ centerY
            , width (fill |> minimum 100)
            , alignRight
            ]

        loginInput =
            Input.text []
                { onChange = ChangeEmailSignin
                , text = editedOrNotEditedValue model.emailSignin
                , placeholder = Just <| Input.placeholder [] (text "email")
                , label = Input.labelLeft labelInputAttributes (text "email")
                }

        passwordInput =
            Input.text []
                { onChange = ChangePasswordSignin
                , text = editedOrNotEditedValue model.passwordSignin
                , placeholder = Just <| Input.placeholder [] (text "password")
                , label = Input.labelLeft labelInputAttributes (text "password")
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
