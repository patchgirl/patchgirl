module Signin.App exposing (..)

import Application.Type exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Html exposing (Html)


-- * model


type alias Model a =
    { a
      | session : Session
      , emailSignin : Editable String
      , passwordSignin : Editable String
    }


-- * message


type Msg
    = ChangeEmailSignin String
    | ChangePasswordSignin String

-- * update


update : Msg -> Model a -> Model a
update msg model =
    case msg of

        ChangeEmailSignin emailSignin ->
            let
                oldEmailSignin = model.emailSignin
                newEmailSignin = changeEditedValue emailSignin oldEmailSignin
            in
                { model | emailSignin = newEmailSignin }

        ChangePasswordSignin passwordSignin ->
            let
                oldPasswordSignin = model.passwordSignin
                newPasswordSignin = changeEditedValue passwordSignin oldPasswordSignin
            in
                { model | passwordSignin = newPasswordSignin }


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
    in
        column [ centerX, spacing 10 ]
            [ loginInput
            , passwordInput
            ]
