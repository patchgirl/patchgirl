module InitializePassword.App exposing (..)

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
import Uuid


-- * model


type alias Model a =
    { a
        | session : Session
        , initializePassword1 : String
        , initializePassword2 : String
        , initializePasswordState : InitializePasswordState
    }


-- * message


type Msg
    = AskInitializePassword Uuid.Uuid String
    | ChangePassword1 String
    | ChangePassword2 String
    | InitializePasswordSucceeded
    | InitializePasswordFailed


-- * update


update : Msg -> Model a -> (Model a, Cmd Msg)
update msg model =
    case msg of
        ChangePassword1 newPassword1 ->
            let
                newModel =
                    { model | initializePassword1 = newPassword1 } |> addError
            in
                (newModel, Cmd.none)

        ChangePassword2 newPassword2 ->
            let
                newModel =
                    { model | initializePassword2 = newPassword2 } |> addError
            in
                (newModel, Cmd.none)

        AskInitializePassword accountId signUpToken ->
            let
                initializePassword =
                    Cmd.none
                        --Client.convertInitializePasswordBackToFront model.initializePassword1 signUpToken accountId

                newCmd =
                    Cmd.none
                        --Client.postApiAccountInitializePassword "" initializePassword postInitializePasswordResultToMsg

            in
                (model, newCmd)

        InitializePasswordFailed ->
            let
                newModel =
                    { model | initializePasswordState = FailedPasswordState "we could not initialize your password" }
            in
                (newModel, Cmd.none)

        InitializePasswordSucceeded ->
            let
                newModel =
                    { model | initializePasswordState = SucceededPasswordState }
            in
                (newModel, Cmd.none)



-- * util


addError : Model a -> Model a
addError model =
    case model.initializePassword1 /= model.initializePassword2 of
        True ->
            { model | initializePasswordState = FailedPasswordState "passwords do not match" }

        False ->
            case model.initializePassword1 == "" of
                True ->
                    { model | initializePasswordState = InitialPasswordState }

                False ->
                    { model | initializePasswordState = FilledPasswordState }


postInitializePasswordResultToMsg : Result Http.Error () -> Msg
postInitializePasswordResultToMsg result =
    case result of
        Ok () ->
            InitializePasswordSucceeded

        Err _ ->
            InitializePasswordFailed


-- * view


errorViewWhenAlreadySignedIn : Element Msg
errorViewWhenAlreadySignedIn =
    column [ centerX
           , centerY
           , spacing 10
           ]
    [ el [ centerX ] (text "You cannot initialize an account password while being signed on")
    , el [ centerX ] (text "Please sign out before proceeding")
    ]

view : Uuid.Uuid -> String -> Model a -> Element Msg
view accountId signUpToken model =
    let
        labelInputAttributes =
            [ centerY
            , width (fill |> minimum 100)
            , alignRight
            ]

        password1Input =
            Input.newPassword []
                { onChange = ChangePassword1
                , text = model.initializePassword1
                , placeholder = Just <| Input.placeholder [] (text "password")
                , label = Input.labelLeft labelInputAttributes (text "Password:")
                , show = False
                }

        password2Input =
            Input.newPassword []
                { onChange = ChangePassword2
                , text = model.initializePassword2
                , placeholder = Just <| Input.placeholder [] (text "password")
                , label = Input.labelLeft labelInputAttributes (text "Repeat password:")
                , show = False
                }

        showMessage =
            case model.initializePasswordState of
                FailedPasswordState error ->
                    el [ centerX ] (text error)

                InitialPasswordState ->
                    none

                FilledPasswordState ->
                    none

                SucceededPasswordState ->
                    el [centerX] (text "Password initialized successfuly, try signing in!")


        submitButton =
            case model.initializePasswordState of
                FilledPasswordState ->
                    Input.button [ Border.solid
                                 , Border.color secondaryColor
                                 , Border.width 1
                                 , Border.rounded 5
                                 , alignBottom
                                 , Background.color secondaryColor
                                 , paddingXY 10 10
                                 , centerX
                                 ]
                        { onPress = Just (AskInitializePassword accountId signUpToken)
                        , label =
                            row [ centerX ]
                                [ el [] (text "Initialize Password")
                                ]
                        }

                _ ->
                    none

    in
        case model.session of
            Visitor _ ->
                column [ centerX, spacing 20 ]
                    [ password1Input
                    , password2Input
                    , showMessage
                    , submitButton
                    ]


            _ ->
                errorViewWhenAlreadySignedIn
